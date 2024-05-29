package com.veadan.folib.components.artifact;

import cn.hutool.core.date.DatePattern;
import cn.hutool.core.date.DateUtil;
import cn.hutool.core.io.FileUtil;
import cn.hutool.json.JSONUtil;
import com.alibaba.fastjson.JSONObject;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.google.common.collect.Lists;
import com.google.common.collect.Sets;
import com.veadan.folib.artifact.archive.JarArchiveListingFunction;
import com.veadan.folib.artifact.coordinates.DockerArtifactCoordinates;
import com.veadan.folib.artifact.coordinates.PypiArtifactCoordinates;
import com.veadan.folib.cloud.storage.s3fs.S3Path;
import com.veadan.folib.components.DistributedLockComponent;
import com.veadan.folib.components.common.CommonComponent;
import com.veadan.folib.config.NpmLayoutProviderConfig;
import com.veadan.folib.configuration.*;
import com.veadan.folib.constant.GlobalConstants;
import com.veadan.folib.controllers.layout.pypi.PypiBrowsePackageHtmlResponseBuilder;
import com.veadan.folib.data.criteria.Paginator;
import com.veadan.folib.dispatch.ClusterDispatchNodeDto;
import com.veadan.folib.domain.*;
import com.veadan.folib.entity.ArtifactCacheRecord;
import com.veadan.folib.entity.Dict;
import com.veadan.folib.entity.PackageNameBlock;
import com.veadan.folib.enums.*;
import com.veadan.folib.event.artifact.ArtifactEventListenerRegistry;
import com.veadan.folib.event.artifact.ArtifactEventTypeEnum;
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
import com.veadan.folib.services.*;
import com.veadan.folib.storage.metadata.MetadataHelper;
import com.veadan.folib.storage.repository.Repository;
import com.veadan.folib.storage.repository.RepositoryTypeEnum;
import com.veadan.folib.util.CacheUtil;
import com.veadan.folib.util.CommonUtils;
import com.veadan.folib.util.FileSizeConvertUtils;
import com.veadan.folib.util.LocalDateTimeInstance;
import com.veadan.folib.utils.PypiPackageNameConverter;
import com.veadan.folib.utils.VersionUtils;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.codec.digest.DigestUtils;
import org.apache.commons.codec.digest.MessageDigestAlgorithms;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.collections4.MapUtils;
import org.apache.commons.io.FilenameUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.apache.maven.artifact.ArtifactUtils;
import org.apache.maven.artifact.repository.metadata.Metadata;
import org.apache.maven.artifact.repository.metadata.Snapshot;
import org.apache.maven.artifact.repository.metadata.Versioning;
import org.apache.maven.model.Model;
import org.apache.maven.model.io.xpp3.MavenXpp3Reader;
import org.apache.maven.model.io.xpp3.MavenXpp3Writer;
import org.codehaus.plexus.util.xml.pull.XmlPullParserException;
import org.folib.util.Commons;
import org.jsoup.Jsoup;
import org.jsoup.nodes.Document;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Lazy;
import org.springframework.http.HttpStatus;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Component;
import org.springframework.util.Assert;

import javax.inject.Inject;
import javax.ws.rs.client.Client;
import javax.ws.rs.client.WebTarget;
import javax.ws.rs.core.Response;
import java.io.*;
import java.math.BigDecimal;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardOpenOption;
import java.time.LocalDateTime;
import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.Stream;

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

    @Inject
    @Lazy
    private ArtifactEventListenerRegistry artifactEventListenerRegistry;

    @Inject
    @Lazy
    private PackageNameBlockService packageNameBlockService;

    @Inject
    @Lazy
    private ArtifactCacheRecordService artifactCacheRecordService;

    @Inject
    @Qualifier("browseRepositoryDirectoryListingService")
    @Lazy
    private volatile DirectoryListingService directoryListingService;

    @Inject
    @Lazy
    private DistributedLockComponent distributedLockComponent;

    @Inject
    @Lazy
    private ArtifactMetadataService artifactMetadataService;

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
            try (InputStream inputStream = Files.newInputStream(repositoryPath)) {
                S3Path s3Path = (S3Path) repositoryPath.getTarget();
                parentPath = tempPath + File.separator + UUID.randomUUID();
                String filePath = parentPath + File.separator + s3Path.getFileName();
                File tempFile = new File(filePath);
                FileUtil.writeFromStream(inputStream, tempFile);
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
        if (Objects.isNull(repositoryPath)) {
            log.warn("RepositoryPath [{}] does not exist", repositoryPath);
            return false;
        }
        if (repositoryPath.getFileSystem() instanceof DockerFileSystem) {
            log.debug("docker布局");
            String blobs = "blobs";
            String manifest = "manifest";
            String path = repositoryPath.toAbsolutePath().toString();
            if (Boolean.TRUE.equals(block)) {
                if (DockerArtifactCoordinates.include(path)) {
                    flag = true;
                }
            } else if (path.contains("sha256") && !path.contains(blobs) && !path.contains(manifest) && DockerArtifactCoordinates.include(path)) {
                flag = true;
            }
        } else if (repositoryPath.getFileSystem() instanceof MavenFileSystem) {
            log.debug("maven布局");
            if (Boolean.TRUE.equals(scan)) {
                flag = JarArchiveListingFunction.INSTANCE.supports(repositoryPath);
            } else {
                flag = JarArchiveListingFunction.INSTANCE.supports(repositoryPath) || endsWith(repositoryPath.getFileName().toString(), Collections.singletonList(".pom"));
            }
        } else if (repositoryPath.getFileSystem() instanceof NpmFileSystem) {
            log.debug("npm布局");
            List<String> suffixList = Arrays.asList("package.json", ".tgz");
            flag = endsWith(repositoryPath.getFileName().toString(), suffixList);
        } else if (repositoryPath.getFileSystem() instanceof NugetFileSystem) {
            log.debug("nuget布局");
            List<String> suffixList = Arrays.asList(".nupkg", ".nuspec", "packages.config");
            flag = endsWith(repositoryPath.getFileName().toString(), suffixList);
        } else if (repositoryPath.getFileSystem() instanceof PypiFileSystem) {
            log.debug("pypi布局");
            List<String> suffixList = Arrays.asList(".whl", ".egg", ".zip", "tar.gz");
            flag = endsWith(repositoryPath.getFileName().toString(), suffixList);
        } else if (repositoryPath.getFileSystem() instanceof RpmFileSystem) {
            log.debug("rpm布局");
            List<String> suffixList = Collections.singletonList(".rpm");
            flag = endsWith(repositoryPath.getFileName().toString(), suffixList);
        } else if (repositoryPath.getFileSystem() instanceof PhpFileSystem) {
            log.debug("php布局");
            List<String> suffixList = Arrays.asList("tar", "tar.gz", "tar.bz2", "zip");
            flag = endsWith(repositoryPath.getFileName().toString(), suffixList);
        } else if (repositoryPath.getFileSystem() instanceof ConanFileSystem) {
            log.debug("Conan布局");
            List<String> suffixList = Arrays.asList(".tgz", ".py");
            flag = endsWith(repositoryPath.getFileName().toString(), suffixList);
        } else if (repositoryPath.getFileSystem() instanceof HelmFileSystem) {
            List<String> suffixList = Collections.singletonList(".tgz");
            flag = endsWith(repositoryPath.getFileName().toString(), suffixList);
            log.debug("Helm布局");
        } else if (repositoryPath.getFileSystem() instanceof RawFileSystem) {
            log.debug("raw布局");
            if (Boolean.TRUE.equals(scan)) {
                List<String> allSuffixList = Lists.newArrayList(".jar", ".war", ".ear", ".zip", "package.json", ".tgz", ".nupkg", ".nuspec", "packages.config", ".whl", ".egg", ".rpm", "tar", "tar.gz", "tar.bz2", ".py", ".exe", ".podspec");
                flag = endsWith(repositoryPath.getFileName().toString(), allSuffixList);
            } else {
                flag = true;
            }
        } else if (repositoryPath.getFileSystem() instanceof CocoapodsFileSystem) {
            log.debug("cocoapods布局");
            if (Boolean.TRUE.equals(scan)) {
                List<String> allSuffixList = Lists.newArrayList(".tar.gz");
                flag = endsWith(repositoryPath.getFileName().toString(), allSuffixList);
            } else {
                flag = true;
            }
        }
        log.debug("制品路径 [{}] 布局 [{}] 是否是该布局支持的制品类型 [{}]", repositoryPath.toString(), repositoryPath.getRepository().getLayout(), flag);
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
                log.debug("docker布局");
                String blobs = "blobs";
                String manifest = "manifest";
                if (filePath.contains("sha256") && !filePath.contains(blobs) && !filePath.contains(manifest) && DockerArtifactCoordinates.include(filePath)) {
                    flag = true;
                }
            } else if (Maven2LayoutProvider.ALIAS.equals(layout)) {
                log.debug("maven布局");
                flag = endsWith(filePath, Lists.newArrayList(".pom", ".jar", ".war", ".ear"));
            } else if (NpmLayoutProvider.ALIAS.equals(layout)) {
                log.debug("npm布局");
                List<String> suffixList = Arrays.asList("package.json", ".tgz");
                flag = endsWith(filePath, suffixList);
            } else if (NugetLayoutProvider.ALIAS.equals(layout)) {
                log.debug("nuget布局");
                List<String> suffixList = Arrays.asList(".nupkg", ".nuspec", "packages.config");
                flag = endsWith(filePath, suffixList);
            } else if (PypiLayoutProvider.ALIAS.equals(layout)) {
                log.debug("pypi布局");
                List<String> suffixList = Arrays.asList(".whl", ".egg", ".zip", "tar.gz");
                flag = endsWith(filePath, suffixList);
            } else if (RpmLayoutProvider.ALIAS.equals(layout)) {
                log.debug("rpm布局");
                List<String> suffixList = Collections.singletonList(".rpm");
                flag = endsWith(filePath, suffixList);
            } else if (PhpLayoutProvider.ALIAS.equals(layout)) {
                log.debug("php布局");
                List<String> suffixList = Arrays.asList("tar", "tar.gz", "tar.bz2", "zip");
                flag = endsWith(filePath, suffixList);
            } else if (ConanLayoutProvider.ALIAS.equals(layout)) {
                log.debug("Conan布局");
                List<String> suffixList = Arrays.asList(".tgz", ".py");
                flag = endsWith(filePath, suffixList);
            } else if (HelmLayoutProvider.ALIAS.equals(layout)) {
                List<String> suffixList = Collections.singletonList(".tgz");
                flag = endsWith(filePath, suffixList);
                log.debug("Helm布局");
            } else if (RawLayoutProvider.ALIAS.equals(layout)) {
                log.debug("raw布局");
                flag = true;
            } else if (CocoapodsLayoutProvider.ALIAS.equals(layout)) {
                List<String> suffixList = Collections.singletonList(".tar.gz");
                flag = endsWith(filePath, suffixList);
                log.debug("Cocoapods布局");
            }
            log.debug("制品路径 [{}] 布局 [{}] 是否是该布局支持的制品类型 [{}]", filePath, layout, flag);
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
     * @param layout   layout
     * @return true
     */
    public boolean vulnerabilityBlock(Artifact artifact, String layout) {
        if (Objects.isNull(artifact)) {
            return false;
        }
        boolean block = false;
        try {
            String storageId = artifact.getStorageId(), repositoryId = artifact.getRepositoryId();
            if (StringUtils.isBlank(layout)) {
                RootRepositoryPath rootRepositoryPath = repositoryPathResolver.resolve(storageId, repositoryId);
                layout = rootRepositoryPath.getRepository().getLayout();
            }
            boolean isDockerLayout = DockerLayoutProvider.ALIAS.equals(layout);
            Set<Vulnerability> vulnerabilitySet = artifact.getVulnerabilitySet();
            if (isDockerLayout) {
                String manifest = "manifest";
                String path = artifact.getUuid();
                if (DockerArtifactCoordinates.include(path) && path.contains(manifest)) {
                    String keywords = path.substring(path.lastIndexOf("manifest/") + "manifest/".length());
                    vulnerabilitySet = artifactRepository.fetchVulnerabilitiesByKeywords(storageId, repositoryId, keywords);
                }
            }
            Set<String> vulnerabilities = Optional.ofNullable(vulnerabilitySet).orElse(Collections.emptySet()).stream().map(Vulnerability::getUuid).collect(Collectors.toSet());
            final SecurityPolicyConfiguration mutableSecurityPolicyConfiguration = configurationManagementService.getConfiguration().getSecurityPolicyConfiguration();
            if (Objects.nonNull(mutableSecurityPolicyConfiguration)) {
                final Repository repositoryDto = configurationManagementService.getConfiguration().getStorage(storageId).getRepository(repositoryId);
                Set<String> repositoryBlacks = repositoryDto.getVulnerabilityBlacks();
                Set<String> repositoryWhites = repositoryDto.getVulnerabilityWhites();
                Set<String> platformBlacks = mutableSecurityPolicyConfiguration.getBlacks();
                Set<String> platformWhites = mutableSecurityPolicyConfiguration.getWhites();
                if (BlockTypeEnum.ALL.getType().equals(mutableSecurityPolicyConfiguration.getBlockType())) {
                    if (CollectionUtils.isEmpty(vulnerabilitySet)) {
                        return false;
                    }
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
                    if (CollectionUtils.isEmpty(vulnerabilitySet)) {
                        return false;
                    }
                    //黑名单阻断
                    block = vulnerabilities.stream().anyMatch(item -> repositoryBlacks.contains(item) ||
                            (!repositoryWhites.contains(item) && platformBlacks.contains(item)));
                } else if (BlockTypeEnum.PACKAGE_NAME.getType().equals(mutableSecurityPolicyConfiguration.getBlockType())) {
                    //包名阻断
                    List<PackageNameBlock> packageNameBlockList = packageNameBlockService.getPackageNameBlockCache();
                    if (CollectionUtils.isNotEmpty(packageNameBlockList)) {
                        packageNameBlockList = packageNameBlockList.stream().filter(item -> artifact.getArtifactPath().contains(item.getPackageName())).collect(Collectors.toList());
                        if (CollectionUtils.isEmpty(packageNameBlockList)) {
                            return false;
                        }
                        block = packageNameBlockList.stream().anyMatch(packageNameBlock -> {
                            if (ConditionTypeEnum.RANGE.getCondition().equals(packageNameBlock.getConditionValue())) {
                                String artifactVersion = artifact.getArtifactCoordinates().getVersion();
                                if (StringUtils.isBlank(artifactVersion)) {
                                    return false;
                                }
                                long startTime = System.currentTimeMillis();
                                boolean flag = VersionUtils.versionInRange(artifactVersion, packageNameBlock.getVersion());
                                long endTime = System.currentTimeMillis();
                                log.debug("比较版本耗时：[{}] 毫秒", endTime - startTime);
                                return flag;
                            } else if (ConditionTypeEnum.EQ.getCondition().equals(packageNameBlock.getConditionValue())) {
                                String artifactVersion = artifact.getArtifactCoordinates().getVersion();
                                if (StringUtils.isBlank(artifactVersion)) {
                                    return false;
                                }
                                return artifact.getArtifactPath().contains(packageNameBlock.getPackageName()) && artifactVersion.equals(packageNameBlock.getVersion());
                            }
                            return artifact.getArtifactPath().contains(packageNameBlock.getPackageName());
                        });
                    }
                }
            }
        } catch (Exception ex) {
            log.warn("判断制品 [{}] [{}] [{}] 是否需要阻断错误 [{}]", artifact.getStorageId(), artifact.getRepositoryId(), artifact.getArtifactPath(), ExceptionUtils.getStackTrace(ex));
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
        Repository repository = configurationManager.getRepository(storageId, repositoryId);
        if (Objects.nonNull(repository)) {
            UnionRepositoryConfiguration unionRepositoryConfiguration = repository.getUnionRepositoryConfig();
            if (Objects.nonNull(unionRepositoryConfiguration)) {
                if (Boolean.TRUE.equals(unionRepositoryConfiguration.getEnable()) && CollectionUtils.isNotEmpty(unionRepositoryConfiguration.getUnionTargetRepositories())) {
                    unionTargetRepositoryConfigurations = unionRepositoryConfiguration.getUnionTargetRepositories();
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
        Repository repository = configurationManager.getRepository(storageId, repositoryId);
        if (Objects.nonNull(repository)) {
            return repository;
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
        String lockKey = String.format("%s-%s-%s-%s", "promotion", storageId, repositoryId, artifactPath);
        if (distributedLockComponent.lock(lockKey, GlobalConstants.WAIT_LOCK_TIME)) {
            try {
                Artifact updateArtifact = null;
                if (StringUtils.isNotBlank(storageId) && StringUtils.isNotBlank(repositoryId) && StringUtils.isNotBlank(artifactPath)) {
                    RepositoryPath repositoryPath = repositoryPathResolver.resolve(storageId, repositoryId, artifactPath);
                    if (Objects.nonNull(repositoryPath)) {
                        try {
                            Artifact artifact = repositoryPath.getArtifactEntry();
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
                                log.debug("存储空间： {} 仓库：{} 制品：{} 节点：{} 已存在，移除", updateArtifact.getStorageId(), updateArtifact.getRepositoryId(), artifactPath, node);
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
            } finally {
                distributedLockComponent.unLock(lockKey);
            }
        } else {
            log.warn("Handle artifact promotion status [{}] was not get lock", lockKey);
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
            if (CollectionUtils.isNotEmpty(promotionNodes) && promotionNodes.stream().allMatch(item -> item.contains(PromotionStatusEnum.SUCCESS.getStatus()))) {
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
                if (CollectionUtils.isNotEmpty(promotionNodes) && promotionNodes.stream().allMatch(item -> item.contains(PromotionStatusEnum.SUCCESS.getStatus()))) {
                    updateArtifact.setPromotion(PromotionStatusEnum.SUCCESS.getStatus());
                }
                if (CollectionUtils.isEmpty(promotionNodes)) {
                    updateArtifact.setPromotion(GlobalConstants.DROP);
                    promotionNodes.add(GlobalConstants.DROP);
                }
                updateArtifact.setPromotionNodes(promotionNodes);
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
        log.debug("[{}] getArtifactIdGroup [{}] take time [{}] ms", this.getClass().getSimpleName(), uuid, System.currentTimeMillis() - startTime);
        return artifactIdGroup;
    }

    /**
     * 校验metadata是否过期
     *
     * @param artifactIdGroup artifactIdGroup
     * @return true 过期 false 未过期
     */
    public String getArtifactIdGroupMetadata(ArtifactIdGroup artifactIdGroup) {
        if (Objects.isNull(artifactIdGroup) || StringUtils.isBlank(artifactIdGroup.getMetadata()) || !JSONUtil.isJson(artifactIdGroup.getMetadata())) {
            return "";
        }
        JSONObject metadataJson = JSONObject.parseObject(artifactIdGroup.getMetadata());
        String cacheTimeKey = "cacheTime", metadataKey = "metadata";
        if (metadataJson.containsKey(cacheTimeKey)) {
            Long cacheTimeLong = metadataJson.getLong(cacheTimeKey);
            LocalDateTime cacheTime = Commons.toLocalDateTime(cacheTimeLong);
            long timeout = 1800L;
            LocalDateTime nowDate = LocalDateTimeInstance.now();
            LocalDateTime cacheExpireDate = cacheTime.plusSeconds(timeout);
            if (!cacheExpireDate.isBefore(nowDate)) {
                String data = metadataJson.getString(metadataKey);
                return GlobalConstants.NO_DATA.equals(data) ? "" : data;
            }
        }
        return "";
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
                if (Objects.nonNull(itemPackageFeed) && Objects.nonNull(itemPackageFeed.getVersions())) {
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
     * 查询NpmArtifactIdGroupCache
     *
     * @param repository repository
     * @param artifactId artifactId
     * @return packageFeed
     */
    public String getNpmArtifactIdGroupBinaryCache(Repository repository, String artifactId) {
        String binaryFeed = null;
        if (repository.isGroupRepository()) {
            String repositoryBinaryFeed = "";
            for (String storageAndRepositoryId : repository.getGroupRepositories()) {
                String sId = ConfigurationUtils.getStorageId(repository.getStorage().getId(), storageAndRepositoryId);
                String rId = ConfigurationUtils.getRepositoryId(storageAndRepositoryId);
                repositoryBinaryFeed = getNpmArtifactIdGroupBinary(repository, configurationManager.getRepository(sId, rId), artifactId);
                if (StringUtils.isNotBlank(repositoryBinaryFeed)) {
                    binaryFeed = repositoryBinaryFeed;
                    return binaryFeed;
                }
            }
        } else {
            binaryFeed = getNpmArtifactIdGroupBinary(repository, repository, artifactId);
        }
        return binaryFeed;
    }

    /**
     * 更新ArtifactIdGroup
     *
     * @param artifactIdGroup artifactIdGroup
     * @param metadata        metadata
     */
    public void updateArtifactIdGroup(ArtifactIdGroup artifactIdGroup, String metadata) {
        String uuid = "";
        try {
            long startTime = System.currentTimeMillis();
            if (Objects.nonNull(artifactIdGroup)) {
                uuid = artifactIdGroup.getUuid();
                if (StringUtils.isNotBlank(metadata)) {
                    JSONObject metadataJson = new JSONObject();
                    LocalDateTime nowDate = LocalDateTimeInstance.now();
                    metadataJson.put("cacheTime", Commons.toLong(nowDate));
                    metadataJson.put("metadata", metadata);
                    artifactIdGroup.setMetadata(metadataJson.toJSONString());
                } else {
                    artifactIdGroup.setMetadata(metadata);
                }
                artifactIdGroupRepository.saveOrUpdate(artifactIdGroup);
                log.debug("[{}] updateArtifactIdGroup [{}] take time [{}] ms", this.getClass().getSimpleName(), uuid, System.currentTimeMillis() - startTime);
            }
        } catch (Exception ex) {
            String realMessage = CommonUtils.getRealMessage(ex);
            log.warn("[{}] [{}] updateArtifactIdGroup error [{}]",
                    this.getClass().getSimpleName(), uuid, realMessage);
            if (CommonUtils.catchException(realMessage)) {
                log.warn("[{}] [{}] updateArtifactIdGroup catch error",
                        this.getClass().getSimpleName(), uuid);
            }
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
        String storageId = repository.getStorage().getId(), repositoryId = repository.getId();
        ArtifactIdGroup artifactIdGroup = new ArtifactIdGroupEntity(repository.getStorage().getId(), repository.getId(), artifactId);
        artifactIdGroup = getArtifactIdGroup(artifactIdGroup.getUuid());
        String metadata = getArtifactIdGroupMetadata(artifactIdGroup);
        if (StringUtils.isNotBlank(metadata)) {
            if (JSONUtil.isJson(metadata)) {
                log.info("Npm [{}] [{}] [{}] exists cache", storageId, repositoryId, artifactId);
                try (InputStream inputStream = new ByteArrayInputStream(metadata.getBytes())) {
                    packageFeed = npmJacksonMapper.readValue(inputStream, PackageFeed.class);
                } catch (IOException ex) {
                    log.error("[{}] storage [{}] repository [{}] artifactIdGroup [{}] metadata to packageFeed error [{}]", this.getClass().getSimpleName(), storageId, repositoryId, artifactIdGroup.getUuid(), ExceptionUtils.getStackTrace(ex));
                }
            }
        } else {
            if (RepositoryTypeEnum.PROXY.getType().equals(repository.getType())) {
                packageFeed = npmRepositoryFeatures.handleViewPackage(storageId, repositoryId, artifactId);
            } else if (RepositoryTypeEnum.HOSTED.getType().equals(repository.getType())) {
                packageFeed = handlePackageFeed(repository, artifactId, repositorySearchRequest);
            }
            try {
                String packageFeedJson = GlobalConstants.NO_DATA;
                if (Objects.nonNull(packageFeed)) {
                    packageFeedJson = npmJacksonMapper.writeValueAsString(packageFeed);
                }
                updateArtifactIdGroup(new ArtifactIdGroupEntity(repository.getStorage().getId(), repository.getId(), artifactId), packageFeedJson);
            } catch (JsonProcessingException ex) {
                log.warn("[{}] packageFeed 转换异常 [{}] error [{}]", this.getClass().getSimpleName(), JSONObject.toJSONString(packageFeed), ExceptionUtils.getStackTrace(ex));
            }
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
     * @param sourceRepository 源仓库
     * @param repository       repository
     * @param artifactId       artifactId
     * @return npm binary
     */
    public String getNpmArtifactIdGroupBinary(Repository sourceRepository, Repository repository, String artifactId) {
        String binaryFeed = null;
        long startTime = System.currentTimeMillis();
        String storageId = repository.getStorage().getId(), repositoryId = repository.getId();
        ArtifactIdGroup artifactIdGroup = new ArtifactIdGroupEntity(repository.getStorage().getId(), repository.getId(), artifactId);
        artifactIdGroup = getArtifactIdGroup(artifactIdGroup.getUuid());
        String metadata = getArtifactIdGroupMetadata(artifactIdGroup);
        if (StringUtils.isNotBlank(metadata)) {
            if (JSONUtil.isJson(metadata)) {
                log.info("Npm [{}] [{}] [{}] exists cache", storageId, repositoryId, artifactId);
                binaryFeed = metadata;
                return binaryFeed;
            }
        } else {
            if (RepositoryTypeEnum.PROXY.getType().equals(repository.getType())) {
                binaryFeed = npmRepositoryFeatures.handleViewBinary(sourceRepository, storageId, repositoryId, artifactId);
            } else if (RepositoryTypeEnum.HOSTED.getType().equals(repository.getType())) {
                return null;
            }
            try {
                String npmBinaryJson = GlobalConstants.NO_DATA;
                if (Objects.nonNull(binaryFeed)) {
                    npmBinaryJson = binaryFeed;
                }
                updateArtifactIdGroup(new ArtifactIdGroupEntity(repository.getStorage().getId(), repository.getId(), artifactId), npmBinaryJson);
            } catch (Exception ex) {
                log.warn("Error [{}]", ExceptionUtils.getStackTrace(ex));
            }
        }
        log.info("[{}] getNpmArtifactPackageFeed storageId [{}] repositoryId [{}] artifactId [{}] take time [{}] ms", this.getClass().getSimpleName(), repository.getStorage().getId(), repository.getId(), artifactId, System.currentTimeMillis() - startTime);
        return binaryFeed;
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
                    if (itemObj instanceof String) {
                        html = (String) itemObj;
                        return html;
                    } else {
                        packageFeedSet.addAll((List<PypiSearchResult>) itemObj);
                    }
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
        log.debug("[{}] getPypiArtifactPackageFeed storageId [{}] repositoryId [{}] artifactId [{}] take time [{}] ms", this.getClass().getSimpleName(), repository.getStorage().getId(), repository.getId(), pypiSearchRequest.getPackageName(), System.currentTimeMillis() - startTime);
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

    /**
     * 存储制品元数据文件
     *
     * @param repositoryPath repositoryPath
     */
    public void storeArtifactMetadataFile(RepositoryPath repositoryPath) {
        try {
            if (Objects.nonNull(repositoryPath) && Objects.nonNull(repositoryPath.getArtifactEntry()) && Files.exists(repositoryPath)) {
                String fileName = "." + FilenameUtils.getName(repositoryPath.getFileName().toString()) + ".metadata";
                RepositoryPath artifactRepositoryPath = repositoryPath.getParent().resolve(fileName);
                try (ByteArrayOutputStream byteArrayOutputStream = new ByteArrayOutputStream();
                     ObjectOutputStream objectOutputStream = new ObjectOutputStream(byteArrayOutputStream)) {
                    objectOutputStream.writeObject(repositoryPath.getArtifactEntry());
                    byte[] byteArray = byteArrayOutputStream.toByteArray();
                    Files.write(artifactRepositoryPath, byteArray);
                } catch (Exception ex) {
                    log.warn("写入制品 [{}] 本地缓存.metadata文件错误", ExceptionUtils.getStackTrace(ex));
                }
            }
        } catch (Exception ex) {
            log.warn("StoreArtifactMetadataFile error [{}]", ExceptionUtils.getStackTrace(ex));
        }
    }

    /**
     * 存储制品元数据文件
     *
     * @param repositoryPath repositoryPath
     * @param metadataPath   metadataPath
     */
    public void storeArtifactMetadataFile(RepositoryPath repositoryPath, Path metadataPath) {
        try {
            if (Objects.nonNull(repositoryPath) && Objects.nonNull(repositoryPath.getArtifactEntry()) && Files.exists(repositoryPath)) {
                String fileName = "." + FilenameUtils.getName(repositoryPath.getFileName().toString()) + ".metadata";
                Path artifactRepositoryPath = metadataPath.getParent().resolve(fileName);
                try (ByteArrayOutputStream byteArrayOutputStream = new ByteArrayOutputStream();
                     ObjectOutputStream objectOutputStream = new ObjectOutputStream(byteArrayOutputStream)) {
                    objectOutputStream.writeObject(repositoryPath.getArtifactEntry());
                    byte[] byteArray = byteArrayOutputStream.toByteArray();
                    Files.write(artifactRepositoryPath, byteArray);
                } catch (Exception ex) {
                    log.warn("写入制品 [{}] 本地缓存.metadata文件错误", ExceptionUtils.getStackTrace(ex));
                }
            }
        } catch (Exception ex) {
            log.warn("StoreArtifactMetadataFile error [{}]", ExceptionUtils.getStackTrace(ex));
        }
    }

    public Integer getLatestIndex() {
        Path path = Path.of(getEventParentPath());
        if (Files.exists(path)) {
            try (Stream<Path> pathStream = Files.list(path)) {
                List<Path> pathList = pathStream.sorted().collect(Collectors.toList());
                if (CollectionUtils.isNotEmpty(pathList)) {
                    Path eventPath = pathList.get(pathList.size() - 1);
                    String filename = eventPath.getFileName().toString();
                    filename = FilenameUtils.getBaseName(filename);
                    List<String> nameSplitList = Arrays.asList(filename.split("_"));
                    if (CollectionUtils.isNotEmpty(nameSplitList)) {
                        String index = nameSplitList.get(nameSplitList.size() - 1);
                        return Integer.parseInt(index);
                    }
                }
            } catch (Exception ex) {
                log.warn(ExceptionUtils.getStackTrace(ex));
            }
        }
        return null;
    }

    public String getEventParentPath() {
        return tempPath + File.separator + "artifactEvent";
    }

    public Path getEventPath(Integer index) throws IOException {
        if (Objects.isNull(index)) {
            index = 1;
        }
        String filename = DateUtil.format(DateUtil.date(), DatePattern.PURE_DATE_PATTERN) + "_index_%s.txt";
        String filePath = getEventParentPath() + File.separator + String.format(filename, index);
        log.debug("Event file path [{}]", filePath);
        Path path = Path.of(filePath);
        Files.createDirectories(path.getParent());
        //每个事件文件20M大小
        BigDecimal maxSize = BigDecimal.valueOf(20);
        if (!Files.exists(path)) {
            Files.createFile(path);
        }
        if (FileSizeConvertUtils.convertBytesWithDecimal(Files.size(path), FileUnitTypeEnum.MB.getUnit()).compareTo(maxSize) >= 0) {
            return getEventPath(index + 1);
        }
        return path;
    }

    private void storeEvent(RepositoryPath repositoryPath, ArtifactEventTypeEnum artifactEventTypeEnum) throws IOException {
        Path eventPath = getEventPath(getLatestIndex());
        //追加写模式
        try (BufferedWriter writer = Files.newBufferedWriter(eventPath, StandardCharsets.UTF_8, StandardOpenOption.APPEND)) {
            ArtifactEventRecord artifactEventRecord = ArtifactEventRecord.builder().storageId(repositoryPath.getStorageId()).repositoryId(repositoryPath.getRepositoryId())
                    .artifactPath(RepositoryFiles.relativizePath(repositoryPath)).eventType(artifactEventTypeEnum.getType()).build();
            writer.write(JSONObject.toJSONString(artifactEventRecord) + System.lineSeparator());
        }
    }

    public void afterRead(RepositoryPath repositoryPath) {
        try {
            if (Objects.isNull(repositoryPath) || !RepositoryFiles.isArtifact(repositoryPath)) {
                return;
            }
            long startTime = System.currentTimeMillis();
            storeEvent(repositoryPath, ArtifactEventTypeEnum.EVENT_ARTIFACT_FILE_DOWNLOADED);
            log.debug("Write EVENT_ARTIFACT_FILE_DOWNLOADED take time [{}] ms", System.currentTimeMillis() - startTime);
        } catch (Exception ex) {
            log.error("RepositoryPath afterRead error ", ex);
        }
    }

    public void artifactCache(RepositoryPath repositoryPath) {
        try {
            artifactEventListenerRegistry.dispatchArtifactCacheEvent(repositoryPath);
        } catch (Exception ex) {
            log.error("RepositoryPath artifactCache error ", ex);
        }
    }

    public PackageVersion extractPackageVersion(String packageName, String packageJsonSource)
            throws IOException {
        PackageVersion packageVersion;
        try {
            packageVersion = npmJacksonMapper.readValue(packageJsonSource, PackageVersion.class);
        } catch (JsonProcessingException e) {
            throw new IllegalArgumentException(String.format("Failed to parse package.json info for [%s]", packageName),
                    e);
        }
        Assert.isTrue(packageName.equals(packageVersion.getName()),
                String.format("Package name [%s] don't match with [%s].", packageVersion.getName(), packageName));

        return packageVersion;
    }

    @Async("asyncThreadPoolTaskExecutor")
    public void asyncHandlerArtifactCacheRecord(RepositoryPath repositoryPath, CacheSettings cacheSettings, Path targetPath) {
        handlerArtifactCacheRecord(repositoryPath, cacheSettings, targetPath);
    }

    public void handlerArtifactCacheRecord(RepositoryPath repositoryPath, CacheSettings cacheSettings, Path targetPath) {
        try {
            if (Objects.isNull(repositoryPath)) {
                return;
            }
            String artifactPath = "", md5, sha1, sha256;
            Long size = 0L;
            Artifact artifact = repositoryPath.getArtifactEntry();
            String sourcePath = repositoryPath.toString();
            String storageId = repositoryPath.getStorageId(), repositoryId = repositoryPath.getRepositoryId();
            if (Objects.nonNull(artifact)) {
                artifactPath = artifact.getArtifactPath();
                size = artifact.getSizeInBytes();
                md5 = artifact.getChecksums().getOrDefault(MessageDigestAlgorithms.MD5, "");
                sha1 = artifact.getChecksums().getOrDefault(MessageDigestAlgorithms.SHA_1, "");
                sha256 = artifact.getChecksums().getOrDefault(MessageDigestAlgorithms.SHA_256, "");
            } else {
                String prefix = String.format("/%s/%s/", storageId, repositoryId);
                artifactPath = sourcePath.substring(sourcePath.indexOf(prefix) + prefix.length());
                size = Files.size(repositoryPath);
                md5 = getChecksum(repositoryPath, "md5");
                sha1 = getChecksum(repositoryPath, "sha1");
                sha256 = getChecksum(repositoryPath, "sha256");
            }
            ArtifactCacheRecord artifactCacheRecord = ArtifactCacheRecord.builder().storageId(storageId)
                    .repositoryId(repositoryId).artifactPath(artifactPath).size(size).md5(md5).sha1(sha1).sha256(sha256)
                    .cacheDirectoryPath(cacheSettings.getDirectoryPath()).cachePath(targetPath.toString()).build();
            if (!Files.exists(repositoryPath)) {
                artifactCacheRecordService.verifySourceRepositoryPath(repositoryPath);
                return;
            }
            handlerArtifactCacheRecord(artifactCacheRecord);
        } catch (Exception ex) {
            log.warn("处理制品缓存记录失败：[{}]", ExceptionUtils.getStackTrace(ex));
        }
    }

    public String getChecksum(RepositoryPath repositoryPath, String checksumKey) {
        try {
            Path checksumSha1Path = repositoryPath.resolveSibling(repositoryPath.getFileName().toString() + "." + checksumKey);
            if (Files.exists(checksumSha1Path)) {
                return Files.readString(checksumSha1Path);
            }
        } catch (Exception ex) {
            log.warn(ExceptionUtils.getStackTrace(ex));
        }
        return "";
    }

    public CacheSettings getCacheConfig() {
        CacheUtil<String, CacheSettings> cacheUtil = CacheUtil.getInstance();
        String key = DictTypeEnum.CACHE_SETTINGS.getType();
        CacheSettings cacheSettings = cacheUtil.get(key);
        if (Objects.isNull(cacheSettings)) {
            Dict dict = dictService.selectLatestOneDict(Dict.builder().dictType(DictTypeEnum.CACHE_SETTINGS.getType()).build());
            if (Objects.nonNull(dict)) {
                cacheSettings = JSONObject.parseObject(dict.getDictValue(), CacheSettings.class);
                if (Objects.nonNull(cacheSettings)) {
                    cacheUtil.put(key, cacheSettings);
                    CacheUtil<String, String> cacheUtilPath = CacheUtil.getInstance();
                    String pathKey = "ARTIFACT_CACHE_ROOT_PATH";
                    if (Boolean.TRUE.equals(cacheSettings.isEnabled())) {
                        cacheUtilPath.put(pathKey, cacheSettings.getDirectoryPath());
                    } else {
                        cacheUtilPath.remove(pathKey);
                    }
                }
            }
        }
        return cacheSettings;
    }

    public void handlerArtifactCacheRecord(ArtifactCacheRecord artifactCacheRecord) {
        artifactCacheRecordService.saveOrUpdateArtifactCacheRecord(artifactCacheRecord);
    }

    public List<ArtifactCacheRecord> getArtifactCacheRecord(ArtifactCacheRecord artifactCacheRecord, Integer limit) {
        return artifactCacheRecordService.getArtifactCacheRecord(artifactCacheRecord, null, limit);
    }

    public RepositoryPath getRepositoryPath(String storageId, String repositoryId, String artifactPath) {
        try {
            RepositoryPath repositoryPath = repositoryPathResolver.resolve(storageId, repositoryId, artifactPath);
            Repository repository = getRepository(storageId, repositoryId);
            if (!DockerLayoutProvider.ALIAS.equalsIgnoreCase(repository.getLayout())) {
                //非docker布局
                return repositoryPath;
            }
            if (!Files.isDirectory(repositoryPath)) {
                return null;
            }
            DirectoryListing directoryListing = directoryListingService.fromRepositoryPath(repositoryPath);
            List<FileContent> fileContents = directoryListing.getFiles().stream().filter(file -> DockerArtifactCoordinates.include(file.getName())).collect(Collectors.toList());
            if (CollectionUtils.isEmpty(fileContents)) {
                return null;
            }
            FileContent fileContent = fileContents.get(0);
            return repositoryPathResolver.resolve(storageId, repositoryId, fileContent.getArtifactPath());
        } catch (Exception ex) {
            log.error(ExceptionUtils.getStackTrace(ex));
            return null;
        }
    }

    /**
     * 获取metadata
     *
     * @param repositoryPath repositoryPath
     * @param version        version
     * @return metadata
     */
    private Metadata getMetadata(RepositoryPath repositoryPath, String version) {
        Path metadataPath = null;
        try {
            Metadata metadata = null;
            if (ArtifactUtils.isSnapshot(version)) {
                metadataPath = MetadataHelper.getSnapshotMetadataPath(repositoryPath, version);
            } else {
                metadataPath = MetadataHelper.getMetadataPath(repositoryPath);
            }
            if (Files.exists(metadataPath)) {
                try (InputStream inputStream = Files.newInputStream(metadataPath)) {
                    metadata = artifactMetadataService.getMetadata(inputStream);
                }
            }
            return metadata;
        } catch (Exception ex) {
            log.error("path：{}，getMetadata error：{}", metadataPath, ExceptionUtils.getStackTrace(ex));
            throw new RuntimeException("path：【" + metadataPath + "】getMetadata error");
        }
    }

    /**
     * 计算版本号
     *
     * @param storageId    storageId
     * @param repositoryId repositoryId
     * @param groupId      groupId
     * @param artifactId   artifactId
     * @param version      version
     * @param artifactName artifactName
     * @return 版本号
     */
    private String calcLatestSnapshotVersion(String storageId, String repositoryId, String groupId, String artifactId, String version, String artifactName) {
        if (ArtifactUtils.isSnapshot(version)) {
            String artifactPath = String.format("%s/%s", groupId, artifactId);
            RepositoryPath repositoryPath = repositoryPathResolver.resolve(storageId, repositoryId, artifactPath);
            try {
                int buildNumber = 1;
                Metadata metadata = getMetadata(repositoryPath, version);
                if (Objects.nonNull(metadata)) {
                    Versioning versioning = metadata.getVersioning();
                    if (Objects.nonNull(versioning)) {
                        Snapshot snapshot = versioning.getSnapshot();
                        if (Objects.nonNull(snapshot)) {
                            buildNumber = snapshot.getBuildNumber() + 1;
                        }
                    }
                }
                String timestamp = MetadataHelper.getDateFormatInstance().format(Calendar.getInstance().getTime());
                artifactName = artifactName.replace("SNAPSHOT",
                        timestamp.substring(0, 8) + "." + timestamp.substring(8) + "-" + buildNumber);
            } catch (Exception ex) {
                log.error("path：{}，calcLatestSnapshotVersion error：{}", repositoryPath.toAbsolutePath().toString(), ExceptionUtils.getStackTrace(ex));
                throw new RuntimeException("path 【" + repositoryPath.toAbsolutePath().toString() + "】calcLatestSnapshotVersion error");
            }
        }
        return artifactName;
    }

    public String calcMavenArtifactPath(String storageId, String repositoryId, String groupId, String artifactId, String version, String artifactName) {
        if (groupId.contains(GlobalConstants.POINT)) {
            groupId = groupId.replace(GlobalConstants.POINT, File.separator);
        }
        return String.format("%s/%s/%s/%s", groupId, artifactId, version, calcLatestSnapshotVersion(storageId, repositoryId, groupId, artifactId, version, artifactName));
    }

}
