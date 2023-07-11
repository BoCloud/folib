package com.veadan.folib.components.artifact;

import cn.hutool.core.io.FileUtil;
import com.alibaba.fastjson.JSONObject;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.google.common.collect.Lists;
import com.google.common.collect.Sets;
import com.veadan.folib.artifact.archive.JarArchiveListingFunction;
import com.veadan.folib.artifact.coordinates.PypiArtifactCoordinates;
import com.veadan.folib.cloud.storage.s3fs.S3Path;
import com.veadan.folib.components.common.CommonComponent;
import com.veadan.folib.config.NpmLayoutProviderConfig;
import com.veadan.folib.configuration.*;
import com.veadan.folib.controllers.layout.pypi.PypiBrowsePackageHtmlResponseBuilder;
import com.veadan.folib.data.criteria.Paginator;
import com.veadan.folib.domain.Artifact;
import com.veadan.folib.domain.ArtifactEntity;
import com.veadan.folib.domain.ArtifactIdGroup;
import com.veadan.folib.domain.Vulnerability;
import com.veadan.folib.enums.BlockTypeEnum;
import com.veadan.folib.enums.PromotionStatusEnum;
import com.veadan.folib.npm.metadata.*;
import com.veadan.folib.providers.io.RepositoryFiles;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.providers.io.RepositoryPathResolver;
import com.veadan.folib.providers.io.RootRepositoryPath;
import com.veadan.folib.providers.layout.*;
import com.veadan.folib.providers.repository.RepositoryProvider;
import com.veadan.folib.providers.repository.RepositoryProviderRegistry;
import com.veadan.folib.providers.repository.RepositorySearchRequest;
import com.veadan.folib.pypi.PypiSearchRequest;
import com.veadan.folib.pypi.PypiSearchResult;
import com.veadan.folib.repositories.ArtifactIdGroupRepository;
import com.veadan.folib.repositories.ArtifactRepository;
import com.veadan.folib.repository.NpmRepositoryFeatures;
import com.veadan.folib.repository.PypiRepositoryFeatures;
import com.veadan.folib.service.ProxyRepositoryConnectionPoolConfigurationService;
import com.veadan.folib.services.ArtifactService;
import com.veadan.folib.services.ConfigurationManagementService;
import com.veadan.folib.services.DictService;
import com.veadan.folib.storage.repository.Repository;
import com.veadan.folib.storage.repository.RepositoryDto;
import com.veadan.folib.storage.repository.RepositoryTypeEnum;
import com.veadan.folib.util.CommonUtils;
import com.veadan.folib.utils.PypiPackageNameConverter;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.codec.digest.DigestUtils;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.apache.maven.model.Model;
import org.apache.maven.model.io.xpp3.MavenXpp3Reader;
import org.apache.maven.model.io.xpp3.MavenXpp3Writer;
import org.codehaus.plexus.util.xml.pull.XmlPullParserException;
import org.jsoup.Jsoup;
import org.jsoup.nodes.Document;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Lazy;
import org.springframework.http.HttpStatus;
import org.springframework.stereotype.Component;

import javax.inject.Inject;
import javax.ws.rs.client.Client;
import javax.ws.rs.client.WebTarget;
import javax.ws.rs.core.Response;
import java.io.*;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.*;
import java.util.stream.Collectors;

/**
 * @author leipenghui
 * @date 2022/12/15
 **/
@Slf4j
@Component
public class ArtifactComponent {

    @Value("${folib.temp}")
    private String tempPath;

    @Inject
    @Lazy
    protected RepositoryPathResolver repositoryPathResolver;

    @Inject
    @Lazy
    private ArtifactRepository artifactRepository;

    @Inject
    @Lazy
    private ConfigurationManagementService configurationManagementService;

    @Inject
    @Lazy
    private ArtifactService artifactService;

    @Inject
    @Lazy
    private DictService dictService;

    @Inject
    @Lazy
    private ArtifactIdGroupRepository artifactIdGroupRepository;

    @Inject
    @Lazy
    private NpmRepositoryFeatures npmRepositoryFeatures;

    @Inject
    @Lazy
    private ConfigurationManager configurationManager;

    @Inject
    private ProxyRepositoryConnectionPoolConfigurationService clientPool;

    @Inject
    @Lazy
    private CommonComponent commonComponent;

    @Inject
    @Lazy
    private RepositoryProviderRegistry repositoryProviderRegistry;

    @Inject
    @Lazy
    private NpmPackageSupplier npmPackageSupplier;

    @Inject
    @Lazy
    @NpmLayoutProviderConfig.NpmObjectMapper
    private ObjectMapper npmJacksonMapper;

    @Inject
    @Lazy
    private PypiRepositoryFeatures pypiRepositoryFeatures;

    @Inject
    @Lazy
    private PypiBrowsePackageHtmlResponseBuilder pypiBrowsePackageHtmlResponseBuilder;

    /**
     * 读取文件内容
     *
     * @param storageId    存储空间名称
     * @param repositoryId 仓库名称
     * @param path         制品路径
     * @return 文件内容
     * @throws IOException io异常
     */
    public String readRepositoryPathContent(String storageId, String repositoryId, String path) throws IOException {
        RepositoryPath repositoryPath = repositoryPathResolver.resolve(storageId, repositoryId, path);
        return readRepositoryPathContent(repositoryPath);
    }

    /**
     * 读取文件内容
     *
     * @param repositoryPath 路径
     * @return 文件内容
     * @throws IOException io异常
     */
    public String readRepositoryPathContent(RepositoryPath repositoryPath) throws IOException {
        String artifactContent = "";
        if (repositoryPath.getTarget() instanceof S3Path) {
            String parentPath = "";
            try {
                S3Path s3Path = (S3Path) repositoryPath.getTarget();
                InputStream inputStream = Files.newInputStream(repositoryPath);
                parentPath = tempPath + File.separator + UUID.randomUUID();
                String filePath = parentPath + File.separator + s3Path.getFileName();
                File tempFile = new File(filePath);
                FileUtil.writeFromStream(inputStream, tempFile, true);
                artifactContent = FileUtil.readString(tempFile, StandardCharsets.UTF_8);
            } catch (IOException ex) {
                throw new IOException(ex);
            } finally {
                //删除临时文件
                if (StringUtils.isNotBlank(parentPath)) {
                    FileUtil.del(new File(parentPath));
                }
            }
        } else {
            artifactContent = FileUtil.readString(repositoryPath.toAbsolutePath().toString(), StandardCharsets.UTF_8);
        }
        return artifactContent;
    }

    /**
     * 判断路径是否以某种后缀结尾
     *
     * @param path       路径
     * @param suffixList 后缀列表
     * @return true
     */
    private boolean endsWith(String path, List<String> suffixList) {
        boolean flag = false;
        if (StringUtils.isNotBlank(path) && CollectionUtils.isNotEmpty(suffixList)) {
            for (String item : suffixList) {
                if (path.endsWith(item)) {
                    flag = true;
                    break;
                }
            }
        }
        return flag;
    }

    /**
     * 安全扫描 校验制品类型是否是该布局支持的类型
     *
     * @param repositoryPath 仓库地址
     * @return true 支持 false 不支持
     */
    public boolean layoutSupportsForScan(RepositoryPath repositoryPath) {
        return layoutSupports(repositoryPath, false, true);
    }

    /**
     * 漏洞阻断 校验制品类型是否是该布局支持的类型
     *
     * @param repositoryPath 仓库地址
     * @return true 支持 false 不支持
     */
    public boolean layoutSupportsForBlock(RepositoryPath repositoryPath) {
        return layoutSupports(repositoryPath, true, false);
    }

    /**
     * 通用 docker 支持镜像版本 maven 支持pom
     * 校验制品类型是否是该布局支持的类型
     *
     * @param repositoryPath 仓库地址
     * @return true 支持 false 不支持
     */
    public boolean layoutSupports(RepositoryPath repositoryPath) {
        return layoutSupports(repositoryPath, false, false);
    }

    /**
     * 校验制品类型是否是该布局支持的类型
     *
     * @param repositoryPath 仓库地址
     * @param block          阻断 true
     * @param scan           安全扫描 true
     * @return true 支持 false 不支持
     */
    public boolean layoutSupports(RepositoryPath repositoryPath, Boolean block, Boolean scan) {
        boolean flag = false;
        if (Objects.isNull(repositoryPath) || !Files.exists(repositoryPath)) {
            log.error("RepositoryPath [{}] does not exist", repositoryPath);
            return false;
        }
        try {
            if (RepositoryFiles.isChecksum(repositoryPath)) {
                log.error("LayoutSupports [{}] isChecksum", repositoryPath);
                return false;
            }
        } catch (Exception ex) {
            log.error("LayoutSupports get [{}] isChecksum error [{}]", repositoryPath, ExceptionUtils.getStackTrace(ex));
            return false;
        }
        if (repositoryPath.getFileSystem() instanceof DockerFileSystem) {
            log.info("=====>>>>> docker布局");
            String blobs = "blobs";
            String manifest = "manifest";
            String path = "";
            try {
                path = repositoryPath.toAbsolutePath().toString();
                if (Objects.nonNull(repositoryPath.getArtifactEntry())) {
                    path = repositoryPath.getArtifactEntry().getArtifactPath();
                }
            } catch (Exception ex) {
                log.error("Check docker layoutSupports error：{}", ExceptionUtils.getStackTrace(ex));
                path = repositoryPath.toAbsolutePath().toString();
            }
            if (Boolean.TRUE.equals(block)) {
                if (path.contains("sha256") && !path.endsWith(".sha256")) {
                    flag = true;
                }
            } else if (path.contains("sha256") && !path.contains(blobs) && !path.contains(manifest) && !path.endsWith(".sha256")) {
                flag = true;
            }
        } else if (repositoryPath.getFileSystem() instanceof MavenFileSystem) {
            log.info("=====>>>>> maven布局");
            if (Boolean.TRUE.equals(scan)) {
                flag = JarArchiveListingFunction.INSTANCE.supports(repositoryPath);
            } else {
                flag = JarArchiveListingFunction.INSTANCE.supports(repositoryPath) || endsWith(repositoryPath.getFileName().toString(), Collections.singletonList(".pom"));
            }
        } else if (repositoryPath.getFileSystem() instanceof NpmFileSystem) {
            log.info("=====>>>>> npm布局");
            List<String> suffixList = Arrays.asList(".json", ".tgz");
            flag = endsWith(repositoryPath.getFileName().toString(), suffixList);
        } else if (repositoryPath.getFileSystem() instanceof NugetFileSystem) {
            log.info("=====>>>>> nuget布局");
            List<String> suffixList = Arrays.asList(".nupkg", ".nuspec", "packages.config");
            flag = endsWith(repositoryPath.getFileName().toString(), suffixList);
        } else if (repositoryPath.getFileSystem() instanceof PypiFileSystem) {
            log.info("=====>>>>> pypi布局");
            List<String> suffixList = Arrays.asList(".whl", ".egg", ".zip");
            flag = endsWith(repositoryPath.getFileName().toString(), suffixList);
        } else if (repositoryPath.getFileSystem() instanceof RpmFileSystem) {
            log.info("=====>>>>> rpm布局");
            List<String> suffixList = Collections.singletonList(".rpm");
            flag = endsWith(repositoryPath.getFileName().toString(), suffixList);
        } else if (repositoryPath.getFileSystem() instanceof PhpFileSystem) {
            log.info("=====>>>>> php布局");
            List<String> suffixList = Arrays.asList("tar", "tar.gz", "tar.bz2", "zip", "json");
            flag = endsWith(repositoryPath.getFileName().toString(), suffixList);
        } else if (repositoryPath.getFileSystem() instanceof ConanFileSystem) {
            log.info("=====>>>>> Conan布局");
            List<String> suffixList = Arrays.asList(".tgz", ".py");
            flag = endsWith(repositoryPath.getFileName().toString(), suffixList);
        } else if (repositoryPath.getFileSystem() instanceof HelmFileSystem) {
            List<String> suffixList = Collections.singletonList(".tgz");
            flag = endsWith(repositoryPath.getFileName().toString(), suffixList);
            log.info("=====>>>>> Helm布局");
        } else if (repositoryPath.getFileSystem() instanceof RawFileSystem) {
            log.info("=====>>>>> raw布局");
            if (Boolean.TRUE.equals(scan)) {
                List<String> allSuffixList = Lists.newArrayList(".jar", ".war", ".ear", ".zip", ".json", ".tgz", ".nupkg", ".nuspec", "packages.config", ".whl", ".egg", ".zip", ".rpm", "tar", "tar.gz", "tar.bz2", "zip", "json", ".tgz", ".py", ".tgz");
                flag = endsWith(repositoryPath.getFileName().toString(), allSuffixList);
            } else {
                flag = true;
            }
        }
        log.info("=====>>>>>制品路径 [{}] 布局 [{}] 是否是该布局支持的制品类型 [{}]", repositoryPath.toString(), repositoryPath.getRepository().getLayout(), flag);
        return flag;
    }

    /**
     * 校验文件是否是该布局支持的类型
     *
     * @param layout   布局
     * @param filePath 文件路径
     */
    public boolean layoutSupports(String layout, String filePath) {
        boolean flag = false;
        if (Objects.nonNull(filePath)) {
            if (DockerLayoutProvider.ALIAS.equals(layout)) {
                log.info("=====>>>>> docker布局");
                String blobs = "blobs";
                String manifest = "manifest";
                if (filePath.contains("sha256") && !filePath.contains(blobs) && !filePath.contains(manifest) && !filePath.endsWith(".sha256")) {
                    flag = true;
                }
            } else if (Maven2LayoutProvider.ALIAS.equals(layout)) {
                log.info("=====>>>>> maven布局");
                flag = endsWith(filePath, Lists.newArrayList(".pom", ".jar", ".war", ".ear"));
            } else if (NpmLayoutProvider.ALIAS.equals(layout)) {
                log.info("=====>>>>> npm布局");
                List<String> suffixList = Arrays.asList(".json", ".tgz");
                flag = endsWith(filePath, suffixList);
            } else if (NugetLayoutProvider.ALIAS.equals(layout)) {
                log.info("=====>>>>> nuget布局");
                List<String> suffixList = Arrays.asList(".nupkg", ".nuspec", "packages.config");
                flag = endsWith(filePath, suffixList);
            } else if (PypiLayoutProvider.ALIAS.equals(layout)) {
                log.info("=====>>>>> pypi布局");
                List<String> suffixList = Arrays.asList(".whl", ".egg", ".zip");
                flag = endsWith(filePath, suffixList);
            } else if (RpmLayoutProvider.ALIAS.equals(layout)) {
                log.info("=====>>>>> rpm布局");
                List<String> suffixList = Collections.singletonList(".rpm");
                flag = endsWith(filePath, suffixList);
            } else if (PhpLayoutProvider.ALIAS.equals(layout)) {
                log.info("=====>>>>> php布局");
                List<String> suffixList = Arrays.asList("tar", "tar.gz", "tar.bz2", "zip", "json");
                flag = endsWith(filePath, suffixList);
            } else if (ConanLayoutProvider.ALIAS.equals(layout)) {
                log.info("=====>>>>> Conan布局");
                List<String> suffixList = Arrays.asList(".tgz", ".py");
                flag = endsWith(filePath, suffixList);
            } else if (HelmLayoutProvider.ALIAS.equals(layout)) {
                List<String> suffixList = Collections.singletonList(".tgz");
                flag = endsWith(filePath, suffixList);
                log.info("=====>>>>> Helm布局");
            } else if (RawLayoutProvider.ALIAS.equals(layout)) {
                log.info("=====>>>>> raw布局");
                flag = true;
            }
            log.info("=====>>>>>制品路径 [{}] 布局 [{}] 是否是该布局支持的制品类型 [{}]", filePath, layout, flag);
        }
        return flag;
    }

    /**
     * 拼接url
     *
     * @param repositoryBaseUrl url前缀
     * @param path              url路径
     * @return 拼接后的url
     */
    public String escapeUrl(String repositoryBaseUrl, String path) {
        String baseUrl = repositoryBaseUrl + (repositoryBaseUrl.endsWith("/") ? "" : "/");
        String p = (path.startsWith("/") ? path.substring(1, path.length()) : path);
        return baseUrl + p;
    }

    /**
     * 获取制品元数据
     *
     * @param artifact artifact
     * @return 制品元数据
     */
    public JSONObject getMetadata(Artifact artifact) {
        if (Objects.isNull(artifact)) {
            return null;
        }
        String metadata = artifact.getMetadata();
        JSONObject metadataJson = null;
        if (StringUtils.isNotBlank(metadata)) {
            metadataJson = JSONObject.parseObject(metadata);
        }
        return metadataJson;
    }

    /**
     * 获取docker制品镜像名称
     *
     * @param artifactPath 制品路径
     * @return docker制品镜像名称
     */
    public String getDockerImage(String artifactPath) {
        if (StringUtils.isBlank(artifactPath)) {
            return "";
        }
        String artifactName = artifactPath.substring(0, artifactPath.indexOf("/sha256"));
        String separator = "/";
        String[] dockerArr;
        if (artifactName.contains(separator)) {
            dockerArr = artifactName.split(separator);
            artifactName = dockerArr[0] + ":" + dockerArr[1];
        }
        return artifactName;
    }

    /**
     * 生成pom文件
     *
     * @param groupId    groupId
     * @param artifactId artifactId
     * @param version    version
     * @param pomPath    pomPath
     */
    public void pomGenerator(String groupId, String artifactId, String version, String pomPath) {
        FileWriter fileWriter = null;
        try {
            // 创建Maven项目模型
            Model model = new Model();
            model.setModelVersion("4.0.0");
            model.setGroupId(groupId);
            model.setArtifactId(artifactId);
            model.setVersion(version);
            // 保存POM文件
            MavenXpp3Writer writer = new MavenXpp3Writer();
            fileWriter = new FileWriter(pomPath);
            writer.write(fileWriter, model);
            fileWriter.close();
        } catch (Exception ex) {
            log.error(String.format("groupId：%s, artifactId：%s，version：%s，pomPath：%s，保存pom.xml错误：%s", groupId, artifactId, version, pomPath, ExceptionUtils.getStackTrace(ex)));
            throw new RuntimeException(ex);
        }
    }

    /**
     * 判断是否需要阻断
     *
     * @param artifact 制品
     * @return true
     */
    public boolean vulnerabilityBlock(Artifact artifact) {
        if (Objects.isNull(artifact)) {
            return false;
        }
        boolean block = false;
        String storageId = artifact.getStorageId(), repositoryId = artifact.getRepositoryId();
        RootRepositoryPath rootRepositoryPath = repositoryPathResolver.resolve(storageId, repositoryId);
        boolean isDockerLayout = DockerLayoutProvider.ALIAS.equals(rootRepositoryPath.getRepository().getLayout());
        Set<Vulnerability> vulnerabilitySet = artifact.getVulnerabilitySet();
        if (CollectionUtils.isEmpty(vulnerabilitySet)) {
            return false;
        }
        if (isDockerLayout) {
            String manifest = "manifest";
            String path = artifact.getUuid();
            if (path.contains("sha256") && !path.endsWith(".sha256") && path.contains(manifest)) {
                String keywords = path.substring(path.lastIndexOf("manifest/") + "manifest/".length());
                vulnerabilitySet = artifactRepository.fetchVulnerabilitiesByKeywords(storageId, repositoryId, keywords);
            }
        }
        Set<String> vulnerabilities = vulnerabilitySet.stream().map(Vulnerability::getUuid).collect(Collectors.toSet());
        MutableSecurityPolicyConfiguration mutableSecurityPolicyConfiguration = configurationManagementService.getMutableConfigurationClone().getSecurityPolicyConfiguration();
        if (Objects.nonNull(mutableSecurityPolicyConfiguration)) {
            RepositoryDto repositoryDto = configurationManagementService.getMutableConfigurationClone().getStorage(storageId).getRepository(repositoryId);
            Set<String> repositoryBlacks = repositoryDto.getVulnerabilityBlacks();
            Set<String> repositoryWhites = repositoryDto.getVulnerabilityWhites();
            Set<String> platformBlacks = mutableSecurityPolicyConfiguration.getBlacks();
            Set<String> platformWhites = mutableSecurityPolicyConfiguration.getWhites();
            if (BlockTypeEnum.ALL.getType().equals(mutableSecurityPolicyConfiguration.getBlockType())) {
                //过滤仓库级别黑名单
                block = vulnerabilities.stream().anyMatch(repositoryBlacks::contains);
                if (!block) {
                    Set<String> allSet = Sets.newLinkedHashSet(), blackSet;
                    //不在阻断漏洞等级内的漏洞集合，需要过滤黑名单
                    Set<Vulnerability> unIncludeVulnerabilitySet = Sets.newLinkedHashSet();
                    if (CollectionUtils.isNotEmpty(mutableSecurityPolicyConfiguration.getBlockLevels())) {
                        for (Vulnerability vulnerability : vulnerabilitySet) {
                            //开启白名单过滤
                            if (Boolean.TRUE.equals(mutableSecurityPolicyConfiguration.getFilterWhites())) {
                                //过滤仓库级别白名单、平台级别白名单
                                if (repositoryWhites.contains(vulnerability.getUuid()) || platformWhites.contains(vulnerability.getUuid())) {
                                    continue;
                                }
                            }
                            if (mutableSecurityPolicyConfiguration.getBlockLevels().contains(vulnerability.getHighestSeverityText())) {
                                allSet.add(vulnerability.getUuid());
                            } else {
                                unIncludeVulnerabilitySet.add(vulnerability);
                            }
                        }
                    }
                    //过滤平台级别黑名单
                    blackSet = unIncludeVulnerabilitySet.stream().filter(item -> platformBlacks.contains(item.getUuid())).map(Vulnerability::getUuid).collect(Collectors.toCollection(LinkedHashSet::new));
                    allSet.addAll(blackSet);
                    block = CollectionUtils.isNotEmpty(allSet);
                }
            } else if (BlockTypeEnum.BLACK.getType().equals(mutableSecurityPolicyConfiguration.getBlockType())) {
                //黑名单阻断
                block = vulnerabilities.stream().anyMatch(item -> repositoryBlacks.contains(item) ||
                        (!repositoryWhites.contains(item) && platformBlacks.contains(item)));
            } else if (BlockTypeEnum.PACKAGE_NAME.getType().equals(mutableSecurityPolicyConfiguration.getBlockType())) {
                //包名阻断
                Set<String> packageNames = mutableSecurityPolicyConfiguration.getPackageNames();
                if (CollectionUtils.isNotEmpty(packageNames)) {
                    block = packageNames.stream().anyMatch(packageName -> artifact.getArtifactPath().contains(packageName));
                }
            }
        }
        return block;
    }

    /**
     * 获取联邦仓库
     *
     * @param storageId    存储空间
     * @param repositoryId 仓库名称
     * @return 联邦仓库列表
     */
    public Set<UnionTargetRepositoryConfiguration> getUnionTargetRepositories(String storageId, String repositoryId) {
        Set<UnionTargetRepositoryConfiguration> unionTargetRepositoryConfigurations = null;
        RootRepositoryPath rootRepositoryPath = repositoryPathResolver.resolve(storageId, repositoryId);
        if (Objects.nonNull(rootRepositoryPath)) {
            Repository repository = rootRepositoryPath.getRepository();
            if (Objects.nonNull(repository)) {
                UnionRepositoryConfiguration unionRepositoryConfiguration = repository.getUnionRepositoryConfig();
                if (Objects.nonNull(unionRepositoryConfiguration)) {
                    if (Boolean.TRUE.equals(unionRepositoryConfiguration.getEnable()) && CollectionUtils.isNotEmpty(unionRepositoryConfiguration.getUnionTargetRepositories())) {
                        unionTargetRepositoryConfigurations = unionRepositoryConfiguration.getUnionTargetRepositories();
                    }
                }
            }
        }
        return unionTargetRepositoryConfigurations;
    }

    /**
     * 获取仓库信息
     *
     * @param storageId    存储空间
     * @param repositoryId 仓库名称
     * @return 仓库信息
     */
    public Repository getRepository(String storageId, String repositoryId) {
        RootRepositoryPath rootRepositoryPath = repositoryPathResolver.resolve(storageId, repositoryId);
        if (Objects.nonNull(rootRepositoryPath) && Objects.nonNull(rootRepositoryPath.getRepository())) {
            return rootRepositoryPath.getRepository();
        }
        return null;
    }

    /**
     * 更新晋级状态
     *
     * @param node         节点
     * @param storageId    存储空间
     * @param repositoryId 仓库名称
     * @param artifactPath 制品路径
     * @param promotion    晋级状态
     */
    public void handlerArtifactPromotion(String node, String storageId, String repositoryId, String artifactPath, String promotion) {
        handlerArtifactPromotion(node, null, storageId, repositoryId, artifactPath, promotion);
    }

    /**
     * 更新晋级状态
     *
     * @param node      节点
     * @param artifact  制品
     * @param promotion 晋级状态
     */
    public void handlerArtifactPromotion(String node, Artifact artifact, String promotion) {
        handlerArtifactPromotion(node, artifact, null, null, null, promotion);
    }

    /**
     * 更新晋级状态
     *
     * @param node         节点
     * @param artifact     制品
     * @param storageId    存储空间
     * @param repositoryId 仓库名称
     * @param artifactPath 制品路径
     * @param promotion    晋级状态
     */
    private void handlerArtifactPromotion(String node, Artifact artifact, String storageId, String repositoryId, String artifactPath, String promotion) {
        Artifact updateArtifact = null;
        if (Objects.nonNull(artifact)) {
            updateArtifact = new ArtifactEntity(artifact.getNativeId(), artifact.getStorageId(), artifact.getRepositoryId(), artifact.getUuid(), artifact.getArtifactCoordinates());
            updateArtifact.setPromotionNodes(artifact.getPromotionNodes());
        } else if (StringUtils.isNotBlank(storageId) && StringUtils.isNotBlank(repositoryId) && StringUtils.isNotBlank(artifactPath)) {
            RepositoryPath repositoryPath = repositoryPathResolver.resolve(storageId, repositoryId, artifactPath);
            if (Objects.nonNull(repositoryPath)) {
                try {
                    artifact = repositoryPath.getArtifactEntry();
                    if (Objects.nonNull(artifact)) {
                        updateArtifact = new ArtifactEntity(artifact.getNativeId(), artifact.getStorageId(), artifact.getRepositoryId(), artifact.getUuid(), artifact.getArtifactCoordinates());
                        updateArtifact.setPromotionNodes(artifact.getPromotionNodes());
                    }
                } catch (Exception ex) {
                    log.error("存储空间： {} 仓库：{} 制品：{} 错误：{}", storageId, repositoryId, artifactPath, ExceptionUtils.getStackTrace(ex));
                }
            }
        }
        if (Objects.nonNull(updateArtifact)) {
            if (StringUtils.isBlank(node) || PromotionStatusEnum.FAIL.getStatus().equals(promotion) || PromotionStatusEnum.WAIT.getStatus().equals(promotion)) {
                updateArtifact.setPromotion(promotion);
            }
            Set<String> promotionNodes = updateArtifact.getPromotionNodes();
            if (StringUtils.isNotBlank(node)) {
                if (CollectionUtils.isEmpty(promotionNodes)) {
                    promotionNodes = Sets.newLinkedHashSet();
                }
                Iterator<String> iterable = promotionNodes.iterator();
                String promotionNode = "";
                while (iterable.hasNext()) {
                    promotionNode = iterable.next();
                    if (StringUtils.isNotBlank(promotionNode) && promotionNode.contains(node)) {
                        //节点信息已存在，移除
                        iterable.remove();
                        log.info("存储空间： {} 仓库：{} 制品：{} 节点：{} 已存在，移除", updateArtifact.getStorageId(), updateArtifact.getRepositoryId(), artifactPath, node);
                    }
                }
                promotionNode = String.format("%s,%s", node, promotion);
                promotionNodes.add(promotionNode);
                updateArtifact.setPromotionNodes(promotionNodes);
                log.info("存储空间： {} 仓库：{} 制品：{} 晋级节点：{}", updateArtifact.getStorageId(), updateArtifact.getRepositoryId(), updateArtifact.getUuid(), promotionNodes);
            }
            if (CollectionUtils.isNotEmpty(promotionNodes) && promotionNodes.stream().allMatch(item -> item.contains(PromotionStatusEnum.SUCCESS.getStatus()))) {
                updateArtifact.setPromotion(PromotionStatusEnum.SUCCESS.getStatus());
            }
            artifactService.saveOrUpdateArtifact(updateArtifact);
        }
    }

    /**
     * 检查更新晋级状态
     *
     * @param artifact 节点
     */
    public void checkArtifactPromotion(Artifact artifact) {
        if (Objects.nonNull(artifact)) {
            Set<String> promotionNodes = artifact.getPromotionNodes();
            if (CollectionUtils.isNotEmpty(promotionNodes) && promotionNodes.stream().allMatch(PromotionStatusEnum.SUCCESS.getStatus()::contains)) {
                artifact.setPromotion(PromotionStatusEnum.SUCCESS.getStatus());
                artifactService.saveOrUpdateArtifact(artifact);
            }
        }
    }

    /**
     * 移除制品晋级节点
     *
     * @param artifact artifact
     * @param node     node
     */
    public void deleteArtifactPromotionNode(Artifact artifact, String node) {
        if (Objects.nonNull(artifact) && StringUtils.isNotBlank(node)) {
            Set<String> promotionNodes = artifact.getPromotionNodes();
            if (CollectionUtils.isNotEmpty(promotionNodes)) {
                Artifact updateArtifact = new ArtifactEntity(artifact.getNativeId(), artifact.getStorageId(), artifact.getRepositoryId(), artifact.getUuid(), artifact.getArtifactCoordinates());
                Iterator<String> iterable = promotionNodes.iterator();
                String promotionNode = "";
                while (iterable.hasNext()) {
                    promotionNode = iterable.next();
                    if (StringUtils.isNotBlank(promotionNode) && promotionNode.contains(node)) {
                        //节点信息已存在，移除
                        iterable.remove();
                        log.info("存储空间： {} 仓库：{} 制品：{} 节点：{} 不存在，移除", artifact.getStorageId(), artifact.getRepositoryId(), artifact.getArtifactPath(), node);
                    }
                }
                if (CollectionUtils.isNotEmpty(promotionNodes) && promotionNodes.stream().allMatch(PromotionStatusEnum.SUCCESS.getStatus()::contains)) {
                    updateArtifact.setPromotion(PromotionStatusEnum.SUCCESS.getStatus());
                }
                artifactService.saveOrUpdateArtifact(updateArtifact);
            }
        }
    }

    /**
     * 查询自动晋级阻断是否开启
     *
     * @return true 开启 false 关闭
     */
    public boolean promotionBlock() {
        String key = "PROMOTION_BLOCK";
        String value = System.getProperty(key);
        if (StringUtils.isNotBlank(value)) {
            return Boolean.TRUE.equals(Boolean.valueOf(value));
        }
        return false;
    }

    /**
     * 读取pom
     *
     * @param filePath filePath
     * @return Model
     * @throws IOException            IOException
     * @throws XmlPullParserException XmlPullParserException
     */
    public Model getPom(Path filePath)
            throws IOException, XmlPullParserException {
        try (Reader rr = new FileReader(filePath.toFile())) {
            MavenXpp3Reader reader = new MavenXpp3Reader();
            return reader.read(rr);
        }
    }

    /**
     * 查询ArtifactIdGroup
     *
     * @param uuid uuid
     */
    public ArtifactIdGroup getArtifactIdGroup(String uuid) {
        long startTime = System.currentTimeMillis();
        ArtifactIdGroup artifactIdGroup = artifactIdGroupRepository.findByArtifactIdGroup(uuid);
        log.info("[{}] getArtifactIdGroup [{}] take time [{}] ms", this.getClass().getSimpleName(), uuid, System.currentTimeMillis() - startTime);
        return artifactIdGroup;
    }

    /**
     * 查询NpmArtifactIdGroupCache
     *
     * @param repository       repository
     * @param artifactId       artifactId
     * @param coordinateValues coordinateValues
     * @return packageFeed
     */
    public PackageFeed getNpmArtifactIdGroupCache(Repository repository, String artifactId, Collection<String> coordinateValues, RepositorySearchRequest predicate) {
        PackageFeed packageFeed = null;
        if (repository.isGroupRepository()) {
            PackageFeed itemPackageFeed = null;
            List<PackageFeed> packageFeedList = Lists.newArrayList();
            for (String storageAndRepositoryId : repository.getGroupRepositories()) {
                String sId = ConfigurationUtils.getStorageId(repository.getStorage().getId(), storageAndRepositoryId);
                String rId = ConfigurationUtils.getRepositoryId(storageAndRepositoryId);
                itemPackageFeed = getNpmArtifactPackageFeed(configurationManager.getRepository(sId, rId), artifactId, coordinateValues, predicate);
                if (Objects.nonNull(itemPackageFeed)) {
                    packageFeedList.add(itemPackageFeed);
                }
            }
            if (CollectionUtils.isNotEmpty(packageFeedList)) {
                packageFeed = packageFeedList.stream().max(Comparator.comparing(i -> i.getVersions().getAdditionalProperties().size())).get();
                PackageFeed finalPackageFeed = packageFeed;
                packageFeedList.forEach(p -> {
                    for (Map.Entry<String, PackageVersion> entry : p.getVersions().getAdditionalProperties().entrySet()) {
                        finalPackageFeed.getVersions().setAdditionalProperty(entry.getKey(), entry.getValue());
                    }
                });
            }
            if (Objects.nonNull(packageFeed)) {
                packageFeed.setAdditionalProperty("_rev", generateRevisionHashcode(packageFeed));
            }
        } else {
            packageFeed = getNpmArtifactPackageFeed(repository, artifactId, coordinateValues, predicate);
        }
        return packageFeed;
    }

    /**
     * 更新ArtifactIdGroup
     *
     * @param uuid     uuid
     * @param metadata metadata
     */
    public void updateArtifactIdGroup(String uuid, String metadata) {
        try {
            long startTime = System.currentTimeMillis();
            ArtifactIdGroup artifactIdGroup = getArtifactIdGroup(uuid);
            if (Objects.nonNull(artifactIdGroup)) {
                artifactIdGroup.setMetadata(metadata);
                artifactIdGroupRepository.merge(artifactIdGroup);
                log.info("[{}] updateArtifactIdGroup [{}] take time [{}] ms", this.getClass().getSimpleName(), uuid, System.currentTimeMillis() - startTime);
            }
        } catch (Exception ex) {
            String realMessage = CommonUtils.getRealMessage(ex);
            log.warn("[{}] [{}] updateArtifactIdGroup error [{}]",
                    this.getClass().getSimpleName(), uuid, realMessage);
            if (CommonUtils.catchException(realMessage)) {
                log.warn("[{}] [{}] updateArtifactIdGroup catch error",
                        this.getClass().getSimpleName(), uuid);
                return;
            }
            throw ex;
        }
    }

    /**
     * 查询NpmArtifactPackageFeed
     *
     * @param repository              repository
     * @param artifactId              artifactId
     * @param coordinateValues        coordinateValues
     * @param repositorySearchRequest repositorySearchRequest
     * @return NpmArtifactPackageFeed
     */
    public PackageFeed getNpmArtifactPackageFeed(Repository repository,
                                                 String artifactId,
                                                 Collection<String> coordinateValues, RepositorySearchRequest repositorySearchRequest) {
        PackageFeed packageFeed = null;
        long startTime = System.currentTimeMillis();
        if (RepositoryTypeEnum.PROXY.getType().equals(repository.getType())) {
            packageFeed = npmRepositoryFeatures.handleViewPackage(repository.getStorage().getId(), repository.getId(), artifactId);
        } else if (RepositoryTypeEnum.HOSTED.getType().equals(repository.getType())) {
            packageFeed = handlePackageFeed(repository, artifactId, repositorySearchRequest);
        }
        log.info("[{}] getNpmArtifactPackageFeed storageId [{}] repositoryId [{}] artifactId [{}] coordinateValues [{}] take time [{}] ms", this.getClass().getSimpleName(), repository.getStorage().getId(), repository.getId(), artifactId, coordinateValues, System.currentTimeMillis() - startTime);
        return packageFeed;
    }

    private PackageFeed handlePackageFeed(Repository repository, String packageId, RepositorySearchRequest predicate) {
        final String storageId = repository.getStorage().getId();
        final String repositoryId = repository.getId();
        RepositoryProvider provider = repositoryProviderRegistry.getProvider(repository.getType());
        Paginator paginator = new Paginator();
        paginator.setProperty("version");
        paginator.setUseLimit(Boolean.FALSE);

        List<Path> searchResult = provider.search(storageId, repositoryId, predicate, paginator);
        if (CollectionUtils.isEmpty(searchResult)) {
            return null;
        }
        PackageFeed packageFeed = new PackageFeed();
        packageFeed.setName(packageId);
        packageFeed.setAdditionalProperty("_id", packageId);
        Versions versions = new Versions();
        packageFeed.setVersions(versions);

        Time npmTime = new Time();
        packageFeed.setTime(npmTime);

        DistTags distTags = new DistTags();
        packageFeed.setDistTags(distTags);
        searchResult.stream().map(npmPackageSupplier).forEach(p -> {
            PackageVersion npmPackage = p.getNpmPackage();
            versions.setAdditionalProperty(npmPackage.getVersion(), npmPackage);

            npmTime.setAdditionalProperty(npmPackage.getVersion(), p.getReleaseDate());

            Date created = npmTime.getCreated();
            npmTime.setCreated(created == null || created.before(p.getReleaseDate()) ? p.getReleaseDate() : created);

            Date modified = npmTime.getModified();
            npmTime.setModified(modified == null || modified.before(p.getReleaseDate()) ? p.getReleaseDate()
                    : modified);

            if (p.isLastVersion()) {
                distTags.setLatest(npmPackage.getVersion());
            }

        });
        packageFeed.setAdditionalProperty("_rev", generateRevisionHashcode(packageFeed));
        return packageFeed;
    }

    private String generateRevisionHashcode(PackageFeed packageFeed) {
        String versionsShaSum = packageFeed.getVersions().getAdditionalProperties()
                .values()
                .stream()
                .map(x -> x.getDist().getShasum())
                .collect(Collectors.joining());
        return packageFeed.getVersions().getAdditionalProperties().size() + "-" +
                DigestUtils.sha1Hex(versionsShaSum).substring(0, 16);
    }

    /**
     * 获取Document
     *
     * @param repository 仓库信息
     * @param url        url
     * @return Document
     */
    public Document getDocument(Repository repository, String url) {
        Response response = null;
        int statusCode = 0;
        Document document = null;
        try {
            Client client = clientPool.getRestClient(repository.getStorage().getId(), repository.getId());
            WebTarget target = client.target(url);
            commonComponent.authentication(target, repository.getRemoteRepository().getUsername(), repository.getRemoteRepository().getPassword());
            response = target.request().get();
            statusCode = response.getStatus();
            if (statusCode == HttpStatus.OK.value()) {
                String data = response.readEntity(String.class);
                String separator = "/";
                if (!url.endsWith(separator)) {
                    url = url + separator;
                }
                document = Jsoup.parse(data, url);
            }
        } catch (Exception ex) {
            log.error("[{}] getDoc url [{}] response statusCode [{}] error [{}]", this.getClass().getSimpleName(), url, statusCode, ExceptionUtils.getStackTrace(ex));
        } finally {
            if (Objects.nonNull(response)) {
                response.close();
            }
        }
        return document;
    }

    /**
     * 查询PypiArtifactIdGroupCache
     *
     * @param repository        repository
     * @param pypiSearchRequest pypiSearchRequest
     * @return PypiSearchResult
     */
    public String getPypiArtifactIdGroupCache(Repository repository, PypiSearchRequest pypiSearchRequest) {
        String html = pypiBrowsePackageHtmlResponseBuilder.nouFound();
        Object obj = null;
        if (repository.isGroupRepository()) {
            Object itemObj = null;
            Set<PypiSearchResult> packageFeedSet = Sets.newLinkedHashSet();
            for (String storageAndRepositoryId : repository.getGroupRepositories()) {
                String sId = ConfigurationUtils.getStorageId(repository.getStorage().getId(), storageAndRepositoryId);
                String rId = ConfigurationUtils.getRepositoryId(storageAndRepositoryId);
                itemObj = getPypiArtifactPackageFeed(configurationManager.getRepository(sId, rId), pypiSearchRequest);
                if (Objects.nonNull(itemObj)) {
                    packageFeedSet.addAll((List<PypiSearchResult>) itemObj);
                }
            }
            if (CollectionUtils.isNotEmpty(packageFeedSet)) {
                html = pypiBrowsePackageHtmlResponseBuilder.getProxyHtmlResponse(Lists.newArrayList(packageFeedSet));
            }
        } else {
            obj = getPypiArtifactPackageFeed(repository, pypiSearchRequest);
            if (Objects.nonNull(obj)) {
                if (obj instanceof String) {
                    html = (String) obj;
                } else {
                    html = pypiBrowsePackageHtmlResponseBuilder.getProxyHtmlResponse((List<PypiSearchResult>) obj);
                }
            }
        }
        return html;
    }

    /**
     * 查询NpmArtifactPackageFeed
     *
     * @param repository        repository
     * @param pypiSearchRequest pypiSearchRequest
     * @return PypiSearchResult
     */
    public Object getPypiArtifactPackageFeed(Repository repository,
                                             PypiSearchRequest pypiSearchRequest) {
        Object obj = null;
        long startTime = System.currentTimeMillis();
        if (RepositoryTypeEnum.PROXY.getType().equals(repository.getType())) {
            List<PypiSearchResult> pypiSearchResultList = pypiRepositoryFeatures.fetchRemotePypiSearchResult(repository.getStorage().getId(), repository.getId(), pypiSearchRequest);
            if (CollectionUtils.isNotEmpty(pypiSearchResultList)) {
                obj = pypiSearchResultList;
            }
        } else if (RepositoryTypeEnum.HOSTED.getType().equals(repository.getType())) {
            final String packageNameToDownload = PypiPackageNameConverter.escapeSpecialCharacters(pypiSearchRequest.getPackageName());
            obj = handlePypiLocalRepository(repository, packageNameToDownload);
        }
        log.info("[{}] getPypiArtifactPackageFeed storageId [{}] repositoryId [{}] artifactId [{}] take time [{}] ms", this.getClass().getSimpleName(), repository.getStorage().getId(), repository.getId(), pypiSearchRequest.getPackageName(), System.currentTimeMillis() - startTime);
        return obj;
    }

    private String handlePypiLocalRepository(Repository repository, String packageNameToDownload) {
        String html = null;
        RepositoryProvider provider = repositoryProviderRegistry.getProvider(repository.getType());
        RepositorySearchRequest predicate = new RepositorySearchRequest(packageNameToDownload, Collections.singleton(PypiArtifactCoordinates.WHEEL_EXTENSION));
        Paginator paginator = new Paginator();
        List<Path> searchResult = provider.search(repository.getStorage().getId(), repository.getId(),
                predicate, paginator);
        if (CollectionUtils.isNotEmpty(searchResult)) {
            try {
                html = pypiBrowsePackageHtmlResponseBuilder.getHtmlResponse(searchResult);
            } catch (Exception ex) {
                log.error(ExceptionUtils.getStackTrace(ex));
            }
        }
        return html;
    }
}
