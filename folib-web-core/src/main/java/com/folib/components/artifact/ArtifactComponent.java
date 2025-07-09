/*
 * Folib - [新一代AI制品仓库]
 * Copyright (C) 2025 bocloud.com.cn <folib@beyondcent.com>
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * 本程序是自由软件：您可依据GNU通用公共许可证（GPL-3.0+）条款重新发布和修改，
 * 但禁止任何形式的商业售卖行为（包括但不限于：直接销售、捆绑销售、云服务商用）。
 *
 * This program is distributed WITHOUT ANY WARRANTY.
 * Commercial sale of this software is expressly prohibited.
 *
 * For license details, see: https://www.gnu.org/licenses/gpl-3.0.html
 * 商业授权咨询请联系：folib@beyondcent.com
 */
package com.folib.components.artifact;

import cn.hutool.core.date.DatePattern;
import cn.hutool.core.date.DateUtil;
import cn.hutool.core.io.FileUtil;
import com.alibaba.fastjson.JSONObject;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.google.common.collect.Lists;
import com.folib.artifact.archive.JarArchiveListingFunction;
import com.folib.artifact.coordinates.DockerCoordinates;
import com.folib.components.DistributedLockComponent;
import com.folib.components.common.CommonComponent;
import com.folib.config.NpmLayoutProviderConfig;
import com.folib.configuration.ConfigurationManager;
import com.folib.configuration.ConfigurationUtils;
import com.folib.configuration.UnionRepositoryConfiguration;
import com.folib.configuration.UnionTargetRepositoryConfiguration;
import com.folib.constant.GlobalConstants;
import com.folib.domain.Artifact;
import com.folib.domain.ArtifactEntity;
import com.folib.domain.ArtifactEventRecord;
import com.folib.domain.ArtifactIdGroup;
import com.folib.domain.CacheSettings;
import com.folib.domain.DirectoryListing;
import com.folib.domain.FileContent;
import com.folib.entity.Dict;
import com.folib.enums.DictTypeEnum;
import com.folib.enums.FileUnitTypeEnum;
import com.folib.enums.PromotionStatusEnum;
import com.folib.event.artifact.ArtifactEventListenerRegistry;
import com.folib.event.artifact.ArtifactEventTypeEnum;
import com.folib.npm.metadata.PackageVersion;
import com.folib.providers.io.RepositoryFiles;
import com.folib.providers.io.RepositoryPath;
import com.folib.providers.io.RepositoryPathResolver;
import com.folib.providers.layout.CocoapodsFileSystem;
import com.folib.providers.layout.CocoapodsLayoutProvider;
import com.folib.providers.layout.ConanFileSystem;
import com.folib.providers.layout.ConanLayoutProvider;
import com.folib.providers.layout.DebianLayoutProvider;
import com.folib.providers.layout.DockerFileSystem;
import com.folib.providers.layout.DockerLayoutProvider;
import com.folib.providers.GitFlsFileSystem;
import com.folib.providers.GitLfsLayoutProvider;
import com.folib.providers.layout.GoFileSystem;
import com.folib.providers.layout.GoLayoutProvider;
import com.folib.providers.HelmFileSystem;
import com.folib.providers.HelmLayoutProvider;
import com.folib.providers.layout.HuggingFaceFileSystem;
import com.folib.providers.layout.HuggingFaceLayoutProvider;
import com.folib.providers.layout.Maven2LayoutProvider;
import com.folib.providers.layout.MavenFileSystem;
import com.folib.providers.NpmFileSystem;
import com.folib.providers.NpmLayoutProvider;
import com.folib.providers.NugetFileSystem;
import com.folib.providers.NugetLayoutProvider;
import com.folib.providers.PhpFileSystem;
import com.folib.providers.PhpLayoutProvider;
import com.folib.providers.PubFileSystem;
import com.folib.providers.PubLayoutProvider;
import com.folib.providers.PypiFileSystem;
import com.folib.providers.PypiLayoutProvider;
import com.folib.providers.RawFileSystem;
import com.folib.providers.RawLayoutProvider;
import com.folib.providers.RpmFileSystem;
import com.folib.providers.RpmLayoutProvider;
import com.folib.repositories.ArtifactIdGroupRepository;
import com.folib.repositories.ArtifactRepository;
import com.folib.service.ProxyRepositoryConnectionPoolConfigurationService;
import com.folib.services.ArtifactMetadataService;
import com.folib.services.ArtifactService;
import com.folib.services.ConfigurationManagementService;
import com.folib.services.DictService;
import com.folib.services.DirectoryListingService;
import com.folib.storage.metadata.MetadataHelper;
import com.folib.storage.repository.Repository;
import com.folib.util.CacheUtil;
import com.folib.util.CommonUtils;
import com.folib.util.FileSizeConvertUtils;
import com.folib.util.LocalDateTimeInstance;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
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
import org.ccil.cowan.tagsoup.jaxp.SAXFactoryImpl;
import org.codehaus.plexus.util.xml.pull.XmlPullParserException;
import org.folib.util.Commons;
import org.jsoup.Jsoup;
import org.jsoup.nodes.Document;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Lazy;
import org.springframework.http.HttpStatus;
import org.springframework.stereotype.Component;
import org.springframework.util.Assert;
import org.xml.sax.Attributes;
import org.xml.sax.helpers.DefaultHandler;

import javax.inject.Inject;
import javax.ws.rs.client.Client;
import javax.ws.rs.client.WebTarget;
import javax.ws.rs.core.Response;
import javax.xml.parsers.SAXParser;
import java.io.BufferedWriter;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStream;
import java.io.ObjectOutputStream;
import java.io.Reader;
import java.math.BigDecimal;
import java.net.URL;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardOpenOption;
import java.time.LocalDateTime;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.Objects;
import java.util.Set;
import java.util.UUID;
import java.util.function.Consumer;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/**
 * @author veadan
 * @date 2022/12/15
 **/
@Slf4j
@Component
public class ArtifactComponent {

    @Value("${folib.temp}")
    private String tempPath;

    @Value("${folib.artifactDownloadImmediatelyUpdate:false}")
    private boolean artifactDownloadImmediatelyUpdate;

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
    private ConfigurationManager configurationManager;

    @Inject
    private ProxyRepositoryConnectionPoolConfigurationService clientPool;

    @Inject
    @Lazy
    private CommonComponent commonComponent;

    @Inject
    @Lazy
    @NpmLayoutProviderConfig.NpmObjectMapper
    private ObjectMapper npmJacksonMapper;

    @Inject
    @Lazy
    private ArtifactEventListenerRegistry artifactEventListenerRegistry;

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
        if (!Files.exists(repositoryPath)) {
            return "";
        }
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
        return FileUtil.readString(repositoryPath.toAbsolutePath().toString(), StandardCharsets.UTF_8);
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
        return layoutSupports(repositoryPath, false, true, false);
    }

    /**
     * 漏洞阻断 校验制品类型是否是该布局支持的类型
     *
     * @param repositoryPath 仓库地址
     * @return true 支持 false 不支持
     */
    public boolean layoutSupportsForBlock(RepositoryPath repositoryPath) {
        return layoutSupports(repositoryPath, true, false, false);
    }

    /**
     * 通用 docker 支持镜像版本 maven 支持pom
     * 校验制品类型是否是该布局支持的类型
     *
     * @param repositoryPath 仓库地址
     * @return true 支持 false 不支持
     */
    public boolean layoutSupports(RepositoryPath repositoryPath) {
        return layoutSupports(repositoryPath, false, false, false);
    }

    /**
     * 晋级 校验制品类型是否是该布局支持的类型
     *
     * @param repositoryPath 仓库地址
     * @return true 支持 false 不支持
     */
    public boolean layoutSupportsForPromotion(RepositoryPath repositoryPath) {
        return layoutSupports(repositoryPath, false, false, true);
    }

    /**
     * 校验制品类型是否是该布局支持的类型
     *
     * @param repositoryPath 仓库地址
     * @param block          阻断 true
     * @param scan           安全扫描 true
     * @param promotion      晋级 true
     * @return true 支持 false 不支持
     */
    public boolean layoutSupports(RepositoryPath repositoryPath, Boolean block, Boolean scan, Boolean promotion) {
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
                if (DockerCoordinates.include(path)) {
                    flag = true;
                }
            } else if (path.contains("sha256") && !path.contains(blobs) && !path.contains(manifest) && DockerCoordinates.include(path)) {
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
            if (Boolean.TRUE.equals(promotion)) {
                suffixList = Arrays.asList(".tgz", ".har");
            }
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
        } else if (repositoryPath.getFileSystem() instanceof GoFileSystem) {
            log.debug("Go布局");
            if (Boolean.TRUE.equals(scan)) {
                List<String> allSuffixList = Lists.newArrayList(".mod", ".zip");
                flag = endsWith(repositoryPath.getFileName().toString(), allSuffixList);
            } else {
                flag = true;
            }
        } else if (repositoryPath.getFileSystem() instanceof GitFlsFileSystem) {
            log.debug("GitLfs布局");
            if (Boolean.TRUE.equals(scan)) {
                flag = false;
            } else {
                flag = true;
            }
        } else if (repositoryPath.getFileSystem() instanceof HuggingFaceFileSystem) {
            log.debug("HuggingFaceFile布局");
            if (Boolean.TRUE.equals(scan)) {
                flag = false;
            } else {
                flag = true;
            }
        } else if (repositoryPath.getFileSystem() instanceof PubFileSystem) {
            log.debug("pub布局");
            List<String> allSuffixList = Lists.newArrayList(".tar.gz");
            flag = endsWith(repositoryPath.getFileName().toString(), allSuffixList);
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
                if (filePath.contains("sha256") && !filePath.contains(blobs) && !filePath.contains(manifest) && DockerCoordinates.include(filePath)) {
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
            } else if (GoLayoutProvider.ALIAS.equals(layout)) {
                List<String> suffixList = Lists.newArrayList(".info", ".mod", ".zip");
                flag = endsWith(filePath, suffixList);
                log.debug("Go布局");
            } else if (GitLfsLayoutProvider.ALIAS.equals(layout)) {
                log.debug("GitLfs布局");
                flag = true;
            } else if (HuggingFaceLayoutProvider.ALIAS.equals(layout)) {
                log.debug("HuggingFaceFile布局");
                flag = true;
            } else if (PubLayoutProvider.ALIAS.equals(layout)) {
                log.debug("pub布局");
                List<String> suffixList = Collections.singletonList(".tar.gz");
                flag = endsWith(filePath, suffixList);
            }
            else if (DebianLayoutProvider.ALIAS.equals(layout)) {
                log.debug("debian布局");
                List<String> suffixList = Lists.newArrayList(".deb",".gz");
                flag = endsWith(filePath, suffixList);
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
     * @param packaging  packaging
     */
    public void pomGenerator(String groupId, String artifactId, String version, String pomPath, String packaging) {
        FileWriter fileWriter = null;
        try {
            // 创建Maven项目模型
            Model model = new Model();
            model.setModelVersion("4.0.0");
            model.setGroupId(groupId);
            model.setArtifactId(artifactId);
            model.setVersion(version);
            if (StringUtils.isNotBlank(packaging)) {
                model.setPackaging(packaging);
            }
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
        String parentPath = "";
        try {
            Client client = clientPool.getRestClient(repository.getStorage().getId(), repository.getId());
            WebTarget target = client.target(url);
            commonComponent.authentication(target, repository.getRemoteRepository().getUsername(), repository.getRemoteRepository().getPassword());
            response = target.request().get();
            statusCode = response.getStatus();
            if (statusCode == HttpStatus.OK.value()) {
                InputStream inputStream = response.readEntity(InputStream.class);
                parentPath = tempPath + File.separator + "document" + File.separator + ConfigurationUtils.getSpecialStorageIdAndRepositoryId(repository.getStorage().getId(), repository.getId())
                        + File.separator + StringUtils.removeStart(StringUtils.removeEnd(target.getUri().getPath(), GlobalConstants.SEPARATOR), GlobalConstants.SEPARATOR);
                String filePath = parentPath + File.separator + UUID.randomUUID().toString() + ".html";
                File tempFile = new File(filePath);
                FileUtil.writeFromStream(inputStream, tempFile);
                String separator = "/";
                if (!url.endsWith(separator)) {
                    url = url + separator;
                }
                log.info("Get document url [{}] tempFile [{}] size [{}]", url, tempFile.getAbsolutePath(), tempFile.length());
                document = Jsoup.parse(tempFile, "UTF-8", url);
            } else {
                log.error("Get document url [{}] error response statusCode [{}]", url, statusCode);
            }
        } catch (Exception ex) {
            log.error("Get document url [{}] response statusCode [{}] error [{}]", url, statusCode, ExceptionUtils.getStackTrace(ex));
        } finally {
            if (Objects.nonNull(response)) {
                response.close();
            }
            if (StringUtils.isNotBlank(parentPath)) {
                FileUtil.del(new File(parentPath));
            }
        }
        return document;
    }

    /**
     * 存储制品元数据文件
     *
     * @param repositoryPath repositoryPath
     */
    public void storeArtifactMetadataFile(RepositoryPath repositoryPath) {
        try {
            if (Objects.nonNull(repositoryPath) && Objects.nonNull(repositoryPath.getArtifactEntry()) && Files.exists(repositoryPath)) {
                Artifact artifact = repositoryPath.getArtifactEntry();
                String fileName = "." + FilenameUtils.getName(repositoryPath.getFileName().toString()) + ".metadata";
                RepositoryPath artifactRepositoryPath = repositoryPath.getParent().resolve(fileName);
                try (ByteArrayOutputStream byteArrayOutputStream = new ByteArrayOutputStream();
                     ObjectOutputStream objectOutputStream = new ObjectOutputStream(byteArrayOutputStream)) {
                    objectOutputStream.writeObject(artifact);
                    byte[] byteArray = byteArrayOutputStream.toByteArray();
                    Files.write(artifactRepositoryPath, byteArray);
                } catch (Exception ex) {
                    log.debug("写入制品 [{}] 本地缓存.metadata文件错误", ExceptionUtils.getStackTrace(ex));
                }
                if (StringUtils.isNotBlank(artifact.getMetadata())) {
                    cacheArtifactMetadata(repositoryPath, artifact.getMetadata());
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
                    log.debug("写入制品 [{}] 本地缓存.metadata文件错误", ExceptionUtils.getStackTrace(ex));
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
            if (artifactDownloadImmediatelyUpdate) {
                artifactEventListenerRegistry.dispatchArtifactDownloadedEvent(repositoryPath);
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
            List<FileContent> fileContents = directoryListing.getFiles().stream().filter(file -> DockerCoordinates.include(file.getName())).collect(Collectors.toList());
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

    public RepositoryPath getBomRepositoryPath(RepositoryPath repositoryPath) {
        String filename = FilenameUtils.getName(repositoryPath.getFileName().toString());
        String filePath = "." + filename + ".foLibrary-metadata/bom.json";
        return repositoryPath.resolveSibling(filePath);
    }

    public RepositoryPath getCacheArtifactMetadataPath(RepositoryPath repositoryPath) {
        String artifactMetadataDirectoryName = "." + FilenameUtils.getName(repositoryPath.getFileName().toString()) + GlobalConstants.FO_LIBRARY_METADATA;
        return repositoryPath.resolveSibling(artifactMetadataDirectoryName).resolve("metadata.json");
    }

    public String getCacheArtifactMetadata(RepositoryPath repositoryPath) {
        String metadata = "";
        try {
            RepositoryPath cacheArtifactMetadataPath = getCacheArtifactMetadataPath(repositoryPath);
            if (Objects.nonNull(cacheArtifactMetadataPath) && Files.exists(cacheArtifactMetadataPath)) {
                metadata = Files.readString(cacheArtifactMetadataPath);
                log.info("Cache artifact metadata path [{}] exists metadata [{}]", cacheArtifactMetadataPath, metadata);
            }
        } catch (Exception ex) {
            log.error("GetCacheArtifactMetadata error [{}]", ExceptionUtils.getStackTrace(ex));
        }
        return metadata;
    }

    public void cacheArtifactMetadata(RepositoryPath repositoryPath, String metadata) {
        try {
            RepositoryPath cacheArtifactMetadataPath = getCacheArtifactMetadataPath(repositoryPath);
            if (Objects.nonNull(cacheArtifactMetadataPath) && StringUtils.isNotBlank(metadata)) {
                Files.writeString(cacheArtifactMetadataPath, metadata);
                log.info("Cache artifact metadata path [{}] success metadata [{}]", cacheArtifactMetadataPath, metadata);
            }
        } catch (Exception ex) {
            log.error("StoreArtifactMetadata error [{}]", ExceptionUtils.getStackTrace(ex));
        }
    }


    public void getArtifactByUrl(Repository repository, String url, String dist) {
        Response response = null;
        int statusCode = 0;
        try {
            Client client = clientPool.getRestClient(repository.getStorage().getId(), repository.getId());
            WebTarget target = client.target(url);
            commonComponent.authentication(target, repository.getRemoteRepository().getUsername(), repository.getRemoteRepository().getPassword());
            response = target.request().get();
            statusCode = response.getStatus();
            if (statusCode == HttpStatus.OK.value()) {
                Path path = Paths.get(dist);
                Path parentDir = path.getParent();

                if (parentDir != null && !Files.exists(parentDir)) {
                    Files.createDirectories(parentDir); // 创建父目录
                }
                try (InputStream is = response.readEntity(InputStream.class);
                     FileOutputStream os = new FileOutputStream(dist)) {
                    byte[] buffer = new byte[4096];
                    int bytesRead;
                    while ((bytesRead = is.read(buffer)) != -1) {
                        os.write(buffer, 0, bytesRead);
                    }
                }
            } else {
                log.error("Get artifact url [{}] error response statusCode [{}]", url, statusCode);
            }
        } catch (Exception ex) {
            log.error("Get artifact url [{}] response statusCode [{}] error [{}]", url, statusCode, ExceptionUtils.getStackTrace(ex));
        } finally {
            if (Objects.nonNull(response)) {
                response.close();
            }
        }
    }

    public boolean parseLinksStreaming(Repository repository, String url, Consumer<String> linkConsumer) {
        Response response = null;
        int statusCode = 0;
        try {
            log.info("Get document url [{}]", url);
            SAXFactoryImpl factory = new SAXFactoryImpl();
            SAXParser saxParser = factory.newSAXParser();
            Client client = clientPool.getRestClient(repository.getStorage().getId(), repository.getId());
            WebTarget target = client.target(url);
            commonComponent.authentication(target, repository.getRemoteRepository().getUsername(), repository.getRemoteRepository().getPassword());
            response = target.request().get();
            statusCode = response.getStatus();
            if (statusCode == HttpStatus.OK.value()) {
                InputStream inputStream = response.readEntity(InputStream.class);
                DefaultHandler handler = new DefaultHandler() {
                    private StringBuilder content = new StringBuilder();
                    private boolean isATag = false;
                    private String href = "";
                    @Override
                    public void startElement(String uri, String localName, String qName, Attributes attributes) {
                        if ("a".equalsIgnoreCase(qName)) {
                            // 标识进入 <a> 标签
                            isATag = true;
                            href = attributes.getValue("href");
                            log.info("StorageId [{}] repositoryId [{}] href [{}]", repository.getStorage().getId(), repository.getId(), href);
                        }
                    }

                    @Override
                    public void characters(char[] ch, int start, int length) {
                        if (isATag) {
                            // 捕获 <a> 标签内的文本内容
                            content.append(ch, start, length);
                        }
                    }

                    @Override
                    public void endElement(String uri, String localName, String qName) {
                        if ("a".equalsIgnoreCase(qName)) {
                            // 标识离开 <a> 标签
                            try {
                                String value = content.toString();
                                if (href.endsWith(GlobalConstants.SEPARATOR)) {
                                    value = StringUtils.removeEnd(value, GlobalConstants.SEPARATOR) + GlobalConstants.SEPARATOR;
                                }
                                if ("../".equals(href)) {
                                    value = href;
                                }
                                // 检查是否是相对路径
                                if (isRelativePath(value)) {
                                    // 将相对路径转换为绝对路径
                                    URL absoluteUrl = new URL(new URL(url), value);
                                    linkConsumer.accept(absoluteUrl.toString());
                                }
                            } catch (Exception e) {
                                log.info("Invalid URL: " + uri);
                            } finally {
                                href = "";
                                isATag = false;
                                content.setLength(0);
                            }
                        }
                    }

//                    @Override
//                    public void startElement(String uri, String localName, String qName, Attributes attributes) {
//                        if ("a".equalsIgnoreCase(qName)) {
//                            String href = attributes.getValue("href");
//                            log.info("StorageId [{}] repositoryId [{}] href [{}]", repository.getStorage().getId(), repository.getId(), href);
//                            if (href != null) {
//                                try {
//                                    href = StringUtils.removeStart(href, remoteUrl);
//                                    // 检查是否是相对路径
//                                    if (isRelativePath(href)) {
//                                        // 将相对路径转换为绝对路径
//                                        URL absoluteUrl = new URL(new URL(url), href);
//                                        linkConsumer.accept(absoluteUrl.toString());
//                                    }
//                                } catch (Exception e) {
//                                    log.info("Invalid URL: " + href);
//                                }
//                            }
//                        }
//                    }
                };
                saxParser.parse(inputStream, handler);
                return true;
            } else {
                log.error("Get html url [{}] error response statusCode [{}]", url, statusCode);
                return false;
            }
        } catch (Exception ex) {
            log.error("Get html url [{}] response statusCode [{}] error [{}]", url, statusCode, ExceptionUtils.getStackTrace(ex));
            return false;
        } finally {
            if (Objects.nonNull(response)) {
                response.close();
            }
        }
    }

    public String getHtml(Repository repository, String url) {
        Response response = null;
        String html=null;
        int statusCode = 0;
        String parentPath = "";
        try {
            Client client = clientPool.getRestClient(repository.getStorage().getId(), repository.getId());
            WebTarget target = client.target(url);
            log.info("get html {}",url);
            commonComponent.authentication(target, repository.getRemoteRepository().getUsername(), repository.getRemoteRepository().getPassword());
            response = target.request().get();
            statusCode = response.getStatus();
            if (statusCode == HttpStatus.OK.value()) {
                html = response.readEntity(String.class);
            } else {
                log.error("Get html url [{}] error response statusCode [{}]", url, statusCode);
            }
        } catch (Exception ex) {
            log.error("Get html url [{}] response statusCode [{}] error [{}]", url, statusCode, ExceptionUtils.getStackTrace(ex));
        } finally {
            if (Objects.nonNull(response)) {
                response.close();
            }
            if (StringUtils.isNotBlank(parentPath)) {
                FileUtil.del(new File(parentPath));
            }
        }
        return html;
    }

    private boolean isRelativePath(String href) {
        if (StringUtils.isBlank(href)) {
            return false;
        }
        // 判断是否以 "http://" 或 "https://" 开头
        return !href.startsWith("http://") && !href.startsWith("https://");
    }


}
