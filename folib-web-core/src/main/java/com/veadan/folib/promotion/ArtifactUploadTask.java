package com.veadan.folib.promotion;

import cn.hutool.core.io.FileUtil;
import cn.hutool.core.io.IORuntimeException;
import cn.hutool.core.util.CharsetUtil;
import cn.hutool.extra.compress.CompressUtil;
import cn.hutool.extra.compress.extractor.Extractor;
import cn.hutool.extra.spring.SpringUtil;
import com.alibaba.fastjson.JSONArray;
import com.alibaba.fastjson.JSONObject;
import com.github.junrar.Archive;
import com.github.junrar.exception.RarException;
import com.github.junrar.rarfile.FileHeader;
import com.google.cloud.tools.jib.api.Containerizer;
import com.google.cloud.tools.jib.api.Jib;
import com.google.cloud.tools.jib.api.RegistryImage;
import com.google.cloud.tools.jib.api.TarImage;
import com.veadan.folib.artifact.MavenArtifactUtils;
import com.veadan.folib.artifact.coordinates.NpmArtifactCoordinates;
import com.veadan.folib.artifact.coordinates.PubArtifactCoordinates;
import com.veadan.folib.components.artifact.ArtifactComponent;
import com.veadan.folib.components.layout.DockerComponent;
import com.veadan.folib.domain.ArtifactIdGroupEntity;
import com.veadan.folib.domain.ArtifactParse;
import com.veadan.folib.domain.DockerManifest;
import com.veadan.folib.entity.Dict;
import com.veadan.folib.enums.NpmPacketSuffix;
import com.veadan.folib.enums.NpmSubLayout;
import com.veadan.folib.providers.io.RepositoryFiles;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.providers.io.RepositoryPathResolver;
import com.veadan.folib.providers.layout.*;
import com.veadan.folib.repositories.ArtifactRepository;
import com.veadan.folib.repository.MavenRepositoryFeatures;
import com.veadan.folib.scanner.common.util.SpringContextUtil;
import com.veadan.folib.services.ArtifactManagementService;
import com.veadan.folib.services.ArtifactMetadataService;
import com.veadan.folib.services.DictService;
import com.veadan.folib.services.RepositoryManagementService;
import com.veadan.folib.storage.metadata.MetadataHelper;
import com.veadan.folib.util.CommonUtils;
import com.veadan.folib.util.MessageDigestUtils;
import com.veadan.folib.utils.CompressionFormatUtils;
import lombok.Data;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.compress.archivers.ArchiveEntry;
import org.apache.commons.compress.archivers.tar.TarArchiveEntry;
import org.apache.commons.compress.archivers.tar.TarArchiveInputStream;
import org.apache.commons.compress.archivers.tar.TarArchiveOutputStream;
import org.apache.commons.compress.compressors.gzip.GzipCompressorInputStream;
import org.apache.commons.compress.utils.IOUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.apache.maven.artifact.ArtifactUtils;
import org.apache.maven.artifact.repository.metadata.Metadata;
import org.apache.maven.artifact.repository.metadata.Snapshot;
import org.apache.maven.artifact.repository.metadata.Versioning;
import org.apache.maven.index.artifact.Gav;
import org.apache.maven.model.Model;
import org.springframework.http.ResponseEntity;
import org.springframework.web.multipart.MultipartFile;

import java.io.*;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.*;
import java.util.concurrent.Callable;
import java.util.zip.GZIPInputStream;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;

@Data
@Slf4j
public class ArtifactUploadTask implements Callable<String> {

    private String storageId;
    private String repositoryId;
    private MultipartFile file;
    private InputStream inputStream;
    private RepositoryManagementService repositoryManagementService;
    private RepositoryPathResolver repositoryPathResolver;
    private ArtifactManagementService artifactManagementService;
    private String fileRelativePath;
    private String metaData;
    private PromotionUtil promotionUtil;
    private LayoutProviderRegistry layoutProviderRegistry;
    private ArtifactMetadataService artifactMetadataService;
    private ArtifactRepository artifactRepository;
    private String tempPath;
    private String uuid;
    private MavenRepositoryFeatures mavenRepositoryFeatures;
    private String parseArtifact;
    private ArtifactComponent artifactComponent;
    private RepositoryPath repositoryPath;
    private String imageTag;
    private String baseUrl;
    private String token ;

    public ArtifactUploadTask() {
    }

    public ArtifactUploadTask(String storageId,
                              String repositoryId,
                              MultipartFile file,
                              RepositoryManagementService repositoryManagementService,
                              RepositoryPathResolver repositoryPathResolver,
                              ArtifactManagementService artifactManagementService,
                              PromotionUtil promotionUtil,
                              LayoutProviderRegistry layoutProviderRegistry,
                              ArtifactMetadataService artifactMetadataService,
                              ArtifactRepository artifactRepository,
                              MavenRepositoryFeatures mavenRepositoryFeatures,
                              String tempPath,
                              String fileRelativePath, String metaData, String uuid, String parseArtifact) {
        this.storageId = storageId;
        this.repositoryId = repositoryId;
        this.file = file;
        this.repositoryManagementService = repositoryManagementService;
        this.repositoryPathResolver = repositoryPathResolver;
        this.artifactManagementService = artifactManagementService;
        this.promotionUtil = promotionUtil;
        this.layoutProviderRegistry = layoutProviderRegistry;
        this.artifactMetadataService = artifactMetadataService;
        this.artifactRepository = artifactRepository;
        this.mavenRepositoryFeatures = mavenRepositoryFeatures;
        this.tempPath = tempPath;
        this.fileRelativePath = fileRelativePath;
        this.metaData = metaData;
        this.uuid = uuid;
        this.parseArtifact = parseArtifact;
        this.artifactComponent = SpringUtil.getBean(ArtifactComponent.class);
    }

    public ArtifactUploadTask(String storageId,
                              String repositoryId,
                              InputStream inputStream,
                              RepositoryPathResolver repositoryPathResolver,
                              ArtifactManagementService artifactManagementService,
                              PromotionUtil promotionUtil,
                              LayoutProviderRegistry layoutProviderRegistry,
                              ArtifactMetadataService artifactMetadataService,
                              ArtifactRepository artifactRepository,
                              MavenRepositoryFeatures mavenRepositoryFeatures,
                              String tempPath,
                              String fileRelativePath, String metaData, String uuid, String parseArtifact) {
        this.storageId = storageId;
        this.repositoryId = repositoryId;
        this.inputStream = inputStream;
        this.repositoryPathResolver = repositoryPathResolver;
        this.artifactManagementService = artifactManagementService;
        this.promotionUtil = promotionUtil;
        this.layoutProviderRegistry = layoutProviderRegistry;
        this.artifactMetadataService = artifactMetadataService;
        this.artifactRepository = artifactRepository;
        this.mavenRepositoryFeatures = mavenRepositoryFeatures;
        this.tempPath = tempPath;
        this.fileRelativePath = fileRelativePath;
        this.metaData = metaData;
        this.uuid = uuid;
        this.parseArtifact = parseArtifact;
        this.artifactComponent = SpringUtil.getBean(ArtifactComponent.class);
    }

    public ArtifactUploadTask(String storageId,
                              String repositoryId,
                              MultipartFile file,
                              RepositoryManagementService repositoryManagementService,
                              RepositoryPathResolver repositoryPathResolver,
                              ArtifactManagementService artifactManagementService,
                              PromotionUtil promotionUtil,
                              LayoutProviderRegistry layoutProviderRegistry,
                              ArtifactMetadataService artifactMetadataService,
                              ArtifactRepository artifactRepository,
                              MavenRepositoryFeatures mavenRepositoryFeatures,
                              String tempPath,
                              String fileRelativePath,
                              String metaData,
                              String uuid,
                              String parseArtifact,
                              String imageTag,
                              String baseUrl,
                              String token) {
        this.storageId = storageId;
        this.repositoryId = repositoryId;
        this.file = file;
        this.repositoryManagementService = repositoryManagementService;
        this.repositoryPathResolver = repositoryPathResolver;
        this.artifactManagementService = artifactManagementService;
        this.promotionUtil = promotionUtil;
        this.layoutProviderRegistry = layoutProviderRegistry;
        this.artifactMetadataService = artifactMetadataService;
        this.artifactRepository = artifactRepository;
        this.mavenRepositoryFeatures = mavenRepositoryFeatures;
        this.tempPath = tempPath;
        this.fileRelativePath = fileRelativePath;
        this.metaData = metaData;
        this.uuid = uuid;
        this.parseArtifact = parseArtifact;
        this.artifactComponent = SpringUtil.getBean(ArtifactComponent.class);
        this.imageTag = imageTag;
        this.baseUrl = baseUrl;
        this.token = token;
    }

    @Override
    public String call() {
        String rs = "";
        ArtifactParse artifactParse = null;
        if (StringUtils.isNotBlank(parseArtifact)) {
            artifactParse = JSONObject.parseObject(parseArtifact, ArtifactParse.class);
        }
        InputStream is = null;
        try {
            if (Objects.nonNull(file)) {
                is = file.getInputStream();
            } else if (Objects.nonNull(artifactParse)) {
                if (StringUtils.isBlank(artifactParse.getFilePath())) {
                    throw new IOException("artifact file not found");
                }
                is = Files.newInputStream(Path.of(artifactParse.getFilePath()));
            } else if (Objects.nonNull(inputStream)) {
                is = inputStream;
            }
            RepositoryPath repositoryPath = repositoryPathResolver.resolve(storageId, repositoryId, fileRelativePath);
            if (RepositoryFiles.isChecksum(repositoryPath) || RepositoryFiles.isTrash(repositoryPath)) {
                log.warn(String.format("RepositoryPath：%s is checksum or trash file skip", repositoryPath.toString()));
                return rs;
            }
            String layout = repositoryPath.getRepository().getLayout();
            if (Maven2LayoutProvider.ALIAS.equals(layout)) {
                if (repositoryPath.toString().endsWith("maven-metadata.xml")) {
                    log.warn(String.format("RepositoryPath：%s is metadata file skip", repositoryPath.toString()));
                    return rs;
                }
                handlerMavenLayoutUpload(is, layout, repositoryPath, artifactParse);
            } else if (NpmLayoutProvider.ALIAS.equals(layout)) {
                handlerNpmLayoutUpload(is, layout, repositoryPath);
            } else if (PubLayoutProvider.ALIAS.equals(layout)) {
                handlerPubLayoutUpload(is, layout, repositoryPath);
            }else if(StringUtils.isNotBlank(imageTag) && DockerLayoutProvider.ALIAS.equals(layout)){
                handlerDockerUploadProcess(this.storageId,  this.repositoryId, this.imageTag, this.file,this.baseUrl);

            } else {
                promotionUtil.setMetaData(repositoryPath, metaData);
                artifactManagementService.store(repositoryPath, is);
            }
        } catch (Exception e) {
            log.info("store file：{}，error：{}", fileRelativePath, ExceptionUtils.getStackTrace(e));
            rs = CommonUtils.getRealMessage(e);
            handlerUploadProcess(rs);
        } finally {
            if (Objects.nonNull(artifactParse) && StringUtils.isNotBlank(artifactParse.getFilePath())) {
                try {
                    FileUtil.del(Path.of(artifactParse.getFilePath()).getParent());
                } catch (IORuntimeException ex) {
                    log.info("store file close：{}，error：{}", fileRelativePath, ExceptionUtils.getStackTrace(ex));
                }
            }
            if (Objects.nonNull(is)) {
                try {
                    is.close();
                } catch (Exception ex) {
                    log.info("store file close：{}，error：{}", fileRelativePath, ExceptionUtils.getStackTrace(ex));
                    rs = CommonUtils.getRealMessage(ex);
                    handlerUploadProcess(rs);
                }
            }
        }
        return rs;
    }

    /**
     * 处理maven布局制品上传
     *
     * @param is             is
     * @param layout         layout
     * @param repositoryPath repositoryPath
     * @param artifactParse  制品信息
     */
    private void handlerMavenLayoutUpload(InputStream is, String layout, RepositoryPath repositoryPath, ArtifactParse artifactParse) {
        File parentTempFile = null;
        try {
            String point = ".";
            //maven布局
            parentTempFile = new File(tempPath + File.separator + UUID.randomUUID() + File.separator);
            File artifactTempFile = new File(parentTempFile.getAbsolutePath() + File.separator + fileRelativePath);
            Path path = Path.of(artifactTempFile.getAbsolutePath());
            FileUtil.writeFromStream(is, artifactTempFile);
            boolean isPom = path.toString().endsWith(".pom");
            LayoutProvider layoutProvider = layoutProviderRegistry.getProvider(layout);
            if (Objects.nonNull(layoutProvider)) {
                if (isPom) {
                    handlerPom(artifactTempFile, point);
                } else {
                    handlerJar(layoutProvider, repositoryPath, path, point, artifactTempFile, parentTempFile, artifactParse);
                }
            }
        } catch (Exception ex) {
            log.error("handlerMavenLayoutUpload path：{}，error：{}", repositoryPath.toAbsolutePath(), ExceptionUtils.getStackTrace(ex));
            throw new RuntimeException(ex.getMessage());
        } finally {
            if (Objects.nonNull(parentTempFile)) {
                FileUtil.del(parentTempFile);
            }
        }
    }

    private String convertArtifactUploadFileName(String name) {
        if (StringUtils.isNotBlank(name)) {
            String[] array = name.split("/");
            if (array.length >= 3) {
                return array[array.length - 1];
            }
        }
        return name;
    }


    /**
     * 处理pom
     *
     * @param artifactTempFile artifactTempFile
     * @param point            point
     * @throws Exception 异常
     */
    private void handlerPom(File artifactTempFile, String point) throws Exception {
        RepositoryPath pomRepositoryPath = null;
        Model model = artifactComponent.getPom(Path.of(artifactTempFile.getAbsolutePath()));
        String groupId = model.getGroupId();
        if (StringUtils.isBlank(groupId) && Objects.nonNull(model.getParent())) {
            groupId = model.getParent().getGroupId();
        }
        if (StringUtils.isBlank(groupId)) {
            throw new RuntimeException("groupId not found");
        }
        String artifactId = model.getArtifactId();
        if (StringUtils.isBlank(artifactId)) {
            throw new RuntimeException("artifactId not found");
        }
        String version = model.getVersion();
        if (StringUtils.isBlank(version) && Objects.nonNull(model.getParent())) {
            version = model.getParent().getVersion();
        }
        if (StringUtils.isBlank(version)) {
            throw new RuntimeException("version not found");
        }
        if (groupId.contains(point)) {
            groupId = groupId.replace(point, File.separator);
        }
//        if (artifactId.contains(point)) {
//            artifactId = artifactId.replace(point, File.separator);
//        }
        fileRelativePath = convertArtifactUploadFileName(fileRelativePath);

        fileRelativePath = calcLatestSnapshotVersion(storageId, repositoryId, groupId, artifactId, version, fileRelativePath);

        String pomPath = String.format("%s/%s/%s/%s", groupId, artifactId, version, fileRelativePath);
        log.info("maven2 layout groupId：{}，artifactId：{}, version：{} artifactPath：{}", groupId, artifactId, version, pomPath);
        pomRepositoryPath = repositoryPathResolver.resolve(storageId, repositoryId, pomPath);
        boolean isValidGavPath = MavenArtifactUtils.isGAV(pomRepositoryPath);
        if (!isValidGavPath) {
            throw new RuntimeException("The artifact is invalid");
        }
        try (InputStream pomInputStream = new BufferedInputStream(FileUtil.getInputStream(artifactTempFile))) {
            promotionUtil.setMetaData(pomRepositoryPath, metaData);
            handlerMavenStore(pomRepositoryPath, pomInputStream);
        } catch (Exception ex) {
            log.error("store pom：{}，error：{}", pomRepositoryPath.toAbsolutePath(), ExceptionUtils.getStackTrace(ex));
            throw new RuntimeException(ex.getMessage());
        }
        try {
            artifactMetadataService.rebuildMetadata(storageId, repositoryId, pomPath);
        } catch (Exception ex) {
            log.error("rebuildMetadata path：{}，error：{}", pomRepositoryPath.toAbsolutePath(), ExceptionUtils.getStackTrace(ex));
        }
    }

    /**
     * 处理jar
     *
     * @param layoutProvider   layoutProvider
     * @param repositoryPath   repositoryPath
     * @param path             path
     * @param point            point
     * @param artifactTempFile artifactTempFile
     * @param parentTempFile   parentTempFile
     * @param artifactParse    制品信息
     */
    private void handlerJar(LayoutProvider layoutProvider, RepositoryPath repositoryPath, Path path, String point, File artifactTempFile, File parentTempFile, ArtifactParse artifactParse) {
        try {
            Gav gav = MavenArtifactUtils.convertPathToGav(fileRelativePath);
            fileRelativePath = convertArtifactUploadFileName(fileRelativePath);
            String groupId, sourceGroupId, artifactId, version, properties = "";
            if (Objects.nonNull(artifactParse)) {
                //优先级最高，非空直接使用
                groupId = artifactParse.getGroupId();
                artifactId = artifactParse.getArtifactId();
                version = artifactParse.getVersion();
            } else if (Objects.nonNull(gav)) {
                groupId = gav.getGroupId();
                artifactId = gav.getArtifactId();
                if (gav.isSnapshot()) {
                    //快照版本
                    version = gav.getBaseVersion();
                } else {
                    version = StringUtils.isNotBlank(gav.getVersion()) ? gav.getVersion() : gav.getBaseVersion();
                }
            } else {
                //路径不包含坐标信息，解析jar中的pom.properties
                byte[] propertiesBytes = layoutProvider.getContentByFileName(repositoryPath, path, "pom.properties");
                if (Objects.isNull(propertiesBytes)) {
                    throw new RuntimeException("Unable to read maven coordinate information, unable to upload");
                }
                properties = new String(propertiesBytes, StandardCharsets.UTF_8);
                groupId = parseProperties(properties, "groupId");
                artifactId = parseProperties(properties, "artifactId");
                version = parseProperties(properties, "version");
            }
            sourceGroupId = groupId;
            if (groupId.contains(point)) {
                groupId = groupId.replace(point, File.separator);
            }
            fileRelativePath = calcLatestSnapshotVersion(storageId, repositoryId, groupId, artifactId, version, fileRelativePath);
            String artifactPath = String.format("%s/%s/%s/%s", groupId, artifactId, version, fileRelativePath);
            log.info("maven2 layout artifact path ：{}，properties：{}，artifactParse: {}, groupId：{}，artifactId：{}, version：{} artifactPath：{}", path, properties, artifactParse, groupId, artifactId, version, artifactPath);
            RepositoryPath artifactRepositoryPath = repositoryPathResolver.resolve(storageId, repositoryId, artifactPath);
            boolean isValidGavPath = MavenArtifactUtils.isGAV(artifactRepositoryPath);
            if (!isValidGavPath) {
                throw new RuntimeException("The artifact is invalid");
            }
            try (InputStream artifactInputStream = new BufferedInputStream(FileUtil.getInputStream(artifactTempFile))) {
                promotionUtil.setMetaData(artifactRepositoryPath, metaData);
                handlerMavenStore(artifactRepositoryPath, artifactInputStream);
            } catch (Exception ex) {
                log.error("store artifact：{}，error：{}", artifactRepositoryPath.toAbsolutePath(), ExceptionUtils.getStackTrace(ex));
                throw new RuntimeException(ex.getMessage());
            }
            Gav artifactGav = MavenArtifactUtils.convertPathToGav(artifactRepositoryPath);
            byte[] pomBytes = layoutProvider.getContentByFileName(repositoryPath, path, "pom.xml");
            String pomName = String.format("%s-%s", artifactId, artifactGav.getVersion()) + ".pom";
            File pomTempFile = null;
            if (Objects.nonNull(pomBytes)) {
                //包内存在pom，直接使用
                String pom = new String(pomBytes, StandardCharsets.UTF_8);
                pomTempFile = new File(parentTempFile.getAbsolutePath() + File.separator + pomName);
                FileUtil.writeBytes(pom.getBytes(), pomTempFile);
            } else {
                //包内不存在pom，需生成pom
                pomName = String.format("%s-%s", artifactId, artifactGav.getVersion()) + ".pom";
                pomTempFile = new File(parentTempFile.getAbsolutePath() + File.separator + pomName);
                artifactComponent.pomGenerator(sourceGroupId, artifactId, version, pomTempFile.getAbsolutePath());
            }
            String pomPath = String.format("%s/%s/%s/%s", groupId, artifactId, version, pomName);
            log.info("maven2 layout xml path ：{}，properties：{}，artifactParse: {}, groupId：{}，artifactId：{}, version：{} gavVersion: {} artifactPath：{}", pomTempFile.getAbsolutePath(), properties, artifactParse, groupId, artifactId, version, artifactGav.getVersion(), pomPath);
            RepositoryPath pomRepositoryPath = repositoryPathResolver.resolve(storageId, repositoryId, pomPath);
            isValidGavPath = MavenArtifactUtils.isGAV(pomRepositoryPath);
            if (!isValidGavPath) {
                throw new RuntimeException("The artifact is invalid");
            }
            try (InputStream pomInputStream = new BufferedInputStream(FileUtil.getInputStream(pomTempFile))) {
                handlerMavenStore(pomRepositoryPath, pomInputStream);
            } catch (Exception ex) {
                log.error("store pom：{}，error：{}", pomRepositoryPath.toAbsolutePath(), ExceptionUtils.getStackTrace(ex));
                throw new RuntimeException(ex.getMessage());
            }
            try {
                artifactMetadataService.rebuildMetadata(storageId, repositoryId, artifactPath);
            } catch (Exception ex) {
                log.error("rebuildMetadata path：{}，error：{}", artifactRepositoryPath.toAbsolutePath(), ExceptionUtils.getStackTrace(ex));
            }
        } catch (Exception ex) {
            log.error("store artifact：{}，error：{}", repositoryPath.toAbsolutePath().toString(), ExceptionUtils.getStackTrace(ex));
            throw new RuntimeException(ex.getMessage());
        }
    }


    /**
     * 处理docker布局制品上传
     *
     * @param is             is
     * @param layout         layout
     * @param repositoryPath repositoryPath
     */
    private void handlerDockerLayoutUpload(InputStream is, String layout, RepositoryPath repositoryPath) {
        File parentTempFile = null;
        try {
            String point = ".";
            //docker布局
            parentTempFile = new File(tempPath + File.separator + UUID.randomUUID() + File.separator);
            File artifactTempFile = new File(parentTempFile.getAbsolutePath() + File.separator + fileRelativePath);
            FileUtil.writeFromStream(is, artifactTempFile);
            //解压tar、tar.gz包
            String imageName = FileUtil.getPrefix(artifactTempFile);
            File extractorFile = new File(parentTempFile.getAbsolutePath() + File.separator + imageName);
            Extractor extractor = CompressUtil.createExtractor(CharsetUtil.defaultCharset(), artifactTempFile);
            extractor.extract(extractorFile);
            String extractorFileRootPath = extractorFile.getAbsolutePath() + File.separator;

            LayoutProvider layoutProvider = layoutProviderRegistry.getProvider(layout);
            if (Objects.nonNull(layoutProvider)) {
                String manifest = FileUtil.readString(extractorFileRootPath + DockerLayoutProvider.IMAGES_MANIFEST, StandardCharsets.UTF_8);
                RuntimeException runtimeException = new RuntimeException("Manifest is not found in this image");
                if (StringUtils.isBlank(manifest)) {
                    throw runtimeException;
                }
                try {
                    List<DockerManifest> imageManifests = JSONArray.parseArray(manifest, DockerManifest.class);
                    if (CollectionUtils.isNotEmpty(imageManifests)) {
                        DockerManifest imageManifest = imageManifests.get(0);
                        String repoTag = imageManifest.getRepoTags().get(0);
                        String tag = repoTag.substring(repoTag.lastIndexOf(":") + 1);
                        log.info("The image：{} version number is：{}", repoTag, tag);
                        Path layerPath = null;
                        String layerDigest = "";
                        RepositoryPath layerRepositoryPath = null;
                        String blobsRootPath = imageName + File.separator + "blobs" + File.separator;
                        for (String layer : imageManifest.getLayers()) {
                            layerPath = Path.of(extractorFileRootPath + layer);
                            log.info("layerPath：{}， size：{}", layerPath, Files.size(layerPath));
                            layerDigest = MessageDigestUtils.calculateChecksum(layerPath, "SHA-256");
                            if (!dockerLayerCheck(imageName, layerDigest)) {
                                layerRepositoryPath = repositoryPathResolver.resolve(storageId, repositoryId, blobsRootPath + layerDigest);
                                log.info("The image layer {} does already exists, store layer：{}", layerPath.toString(), layerRepositoryPath.toString());
                                try (InputStream inputStream = new BufferedInputStream(Files.newInputStream(layerPath))) {
                                    artifactManagementService.store(layerRepositoryPath, inputStream);
                                }
                            } else {
                                log.info("The image layer {} already exists", layerPath.toString());
                            }
                        }
                    } else {
                        throw runtimeException;
                    }
                } catch (Exception ex) {
                    log.error("handlerDockerLayoutUpload file：{}，error：{}", artifactTempFile.getAbsolutePath(), ExceptionUtils.getStackTrace(ex));
                    throw runtimeException;
                }
            }
        } catch (Exception ex) {
            log.error("handlerDockerLayoutUpload path：{}，error：{}", repositoryPath.toAbsolutePath(), ExceptionUtils.getStackTrace(ex));
            throw new RuntimeException(ex.getMessage());
        } finally {
            if (Objects.nonNull(parentTempFile)) {
                FileUtil.del(parentTempFile);
            }
        }
    }

    private boolean dockerLayerCheck(String imageName, String digest) {
        String artifactName = String.format("%s/blobs/%s", imageName, digest);
        return artifactRepository.artifactExists(storageId, repositoryId, artifactName);
    }

    /**
     * 处理npm布局制品上传
     *
     * @param is             is
     * @param layout         layout
     * @param repositoryPath repositoryPath
     */
    private void handlerNpmLayoutUpload(InputStream is, String layout, RepositoryPath repositoryPath) {
        File parentTempFile = null;
        try {
            String supportedExt = "tgz";
            String ohpmExt = "har";
            //npm布局
            parentTempFile = new File(tempPath + File.separator + UUID.randomUUID() + File.separator);
            File artifactTempFile = new File(parentTempFile.getAbsolutePath() + File.separator + fileRelativePath);
            FileUtil.writeFromStream(is, artifactTempFile);
            Path path = Path.of(artifactTempFile.getAbsolutePath());
            String ext = FileUtil.extName(artifactTempFile);

            boolean isOhnpmSubLayout = NpmSubLayout.OHNPM.getValue().equals(repositoryPath.getRepository().getSubLayout());
            String expectedExtension = isOhnpmSubLayout ? ohpmExt : supportedExt;
            if (!expectedExtension.equals(ext)) {
                String errorMessage = isOhnpmSubLayout ? "Only the .har suffix is supported" : "Only the .tgz suffix is supported";
                throw new RuntimeException(errorMessage);
            }

            LayoutProvider layoutProvider = layoutProviderRegistry.getProvider(layout);
            if (Objects.nonNull(layoutProvider)) {
                String packagePath = isOhnpmSubLayout ? NpmLayoutProvider.OHPM_PACKAGE_JSON_PATH : NpmLayoutProvider.DEFAULT_PACKAGE_JSON_PATH;
                byte[] packageJsonBytes = layoutProvider.getContentByEqualsFileName(repositoryPath, path, packagePath);
                String packageJson = new String(packageJsonBytes, StandardCharsets.UTF_8);
                log.info("npm package.json：{}", packageJson);
                RuntimeException runtimeException = new RuntimeException("package.json is not found in this file or package.json has an error");
                if (StringUtils.isBlank(packageJson)) {
                    throw runtimeException;
                }
                try {
                    JSONObject packageJsonObj = JSONObject.parseObject(packageJson);
                    String name = packageJsonObj.getString("name");
                    String version = packageJsonObj.getString("version");
                    if (StringUtils.isBlank(name) || StringUtils.isBlank(version)) {
                        throw runtimeException;
                    }

                    final String packagesuffix = NpmSubLayout.OHNPM.getValue().equals(repositoryPath.getRepository().getSubLayout()) ? NpmPacketSuffix.HAR.getValue() : NpmPacketSuffix.TGZ.getValue();
                    NpmArtifactCoordinates npmArtifactCoordinates = NpmArtifactCoordinates.of(name, version, packagesuffix);
                    String artifactPath = npmArtifactCoordinates.convertToPath(npmArtifactCoordinates);
                    log.info("The fileRelativePath：{} artifactPath：{}", fileRelativePath, artifactPath);
                    repositoryPath = repositoryPathResolver.resolve(storageId, repositoryId, artifactPath);
                    try (InputStream inputStream = new BufferedInputStream(Files.newInputStream(path))) {
                        promotionUtil.setMetaData(repositoryPath, metaData);
                        artifactManagementService.store(repositoryPath, inputStream);
                        this.repositoryPath = repositoryPath;
                    }
                    try (InputStream inputStream = new ByteArrayInputStream(packageJsonBytes)) {
                        artifactManagementService.store(repositoryPath.resolveSibling("package.json"), inputStream);
                    }
                    artifactComponent.updateArtifactIdGroup(new ArtifactIdGroupEntity(storageId, repositoryId, npmArtifactCoordinates.getId()), "");
                } catch (Exception ex) {
                    log.error("handlerNpmLayoutUpload file：{}，error：{}", artifactTempFile.getAbsolutePath(), ExceptionUtils.getStackTrace(ex));
                    throw runtimeException;
                }
            }
        } catch (Exception ex) {
            log.error("handlerNpmLayoutUpload path：{}，error：{}", repositoryPath.toAbsolutePath(), ExceptionUtils.getStackTrace(ex));
            throw new RuntimeException(ex.getMessage());
        } finally {
            if (Objects.nonNull(parentTempFile)) {
                FileUtil.del(parentTempFile);
            }
        }
    }

    /**
     * 读取properties
     *
     * @param propertiesContext 文件内容
     * @param propertiesKey     key
     * @return key对应的值
     */
    private String parseProperties(String propertiesContext, String propertiesKey) {
        try {
            Properties properties = new Properties();
            properties.load(new StringReader(propertiesContext));
            return properties.getProperty(propertiesKey);
        } catch (Exception ex) {
            log.error("parseProperties error：{}", ExceptionUtils.getStackTrace(ex));
            throw new RuntimeException(ex.getMessage());
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

    /**
     * 处理pub布局制品上传
     *
     * @param is             is
     * @param layout         layout
     * @param repositoryPath repositoryPath
     */
    private void handlerPubLayoutUpload(InputStream is, String layout, RepositoryPath repositoryPath) {
        File parentTempFile = null;
        try {
            parentTempFile = new File(tempPath + File.separator + UUID.randomUUID() + File.separator);
            File artifactTempFile = new File(parentTempFile.getAbsolutePath() + File.separator + fileRelativePath);
            FileUtil.writeFromStream(is, artifactTempFile);
            Path path = Path.of(artifactTempFile.getAbsolutePath());
            if (!fileRelativePath.endsWith(PubArtifactCoordinates.PUB_EXTENSION)) {
                String errorMessage = "Only the .tar.gz suffix is supported";
                throw new RuntimeException(errorMessage);
            }

            LayoutProvider layoutProvider = layoutProviderRegistry.getProvider(layout);
            if (Objects.nonNull(layoutProvider)) {
                try {
                    PubArtifactCoordinates pubArtifactCoordinates = PubArtifactCoordinates.packageNameParse(fileRelativePath);
                    String artifactPath = pubArtifactCoordinates.convertToPath(pubArtifactCoordinates);
                    log.info("The fileRelativePath：{} artifactPath：{}", fileRelativePath, artifactPath);
                    repositoryPath = repositoryPathResolver.resolve(storageId, repositoryId, artifactPath);
                    try (InputStream inputStream = new BufferedInputStream(Files.newInputStream(path))) {
                        promotionUtil.setMetaData(repositoryPath, metaData);
                        artifactManagementService.store(repositoryPath, inputStream);
                        this.repositoryPath = repositoryPath;
                    }
                } catch (Exception ex) {
                    log.error("handlerPubLayoutUpload file：{}，error：{}", artifactTempFile.getAbsolutePath(), ExceptionUtils.getStackTrace(ex));
                    throw ex;
                }
            }
        } catch (Exception ex) {
            log.error("handlerPubLayoutUpload path：{}，error：{}", repositoryPath.toAbsolutePath(), ExceptionUtils.getStackTrace(ex));
            throw new RuntimeException(ex.getMessage());
        } finally {
            if (Objects.nonNull(parentTempFile)) {
                FileUtil.del(parentTempFile);
            }
        }
    }

    /**
     * maven仓库
     *
     * @param repositoryPath repositoryPath
     * @param inputStream    inputStream
     * @throws Exception exception
     */
    private void handlerMavenStore(RepositoryPath repositoryPath, InputStream inputStream) throws Exception {
        mavenRepositoryFeatures.versionValidator(repositoryPath);
        artifactManagementService.store(repositoryPath, inputStream);
        if (Objects.isNull(this.repositoryPath)) {
            this.repositoryPath = repositoryPath;
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
     * 处理上传进度异常信息
     *
     * @param comment 异常信息
     */
    private void handlerUploadProcess(String comment) {
        DictService dictService = SpringContextUtil.getBean(DictService.class);
        if (StringUtils.isNotBlank(uuid)) {
            log.error("upload error uuid: {}", uuid);
            if (StringUtils.isBlank(comment)) {
                comment = "未知异常";
            }
            dictService.saveOrUpdateDict(Dict.builder().dictKey(uuid).comment(comment).build(), null);
        }
    }

    private void handlerDockerUploadProcess(final String storageId,  final String repositoryId, final String path, final MultipartFile multipartFile,String  baseUrl){
        log.info("Requested get docker application file {}/{}/{}.", storageId, repositoryId, path);
        String url = String.join("/", baseUrl,storageId, repositoryId, path);

        final String prefix1 = "http://";
        final String prefix2 = "https://";
        String tag = url.replaceAll("^" + prefix1, "");
        if (url.contains(prefix1)) {
            tag = url.replaceAll("^" + prefix1, "");
        } else if (url.contains(prefix2)) {
            tag = url.replaceAll("^" + prefix2, "");
        }
        try {
            String token = this.token;//securityComponent.getSecurityToken();
            String uuid = UUID.randomUUID().toString();
            String TEMP_UPLOAD_DIR = String.join("/",tempPath, uuid);
            // 将文件保存到指定目录下
            String fileName = multipartFile.getOriginalFilename();
            Path tempDirectory = Files.createDirectory(Path.of(TEMP_UPLOAD_DIR));
            Path localPath = Paths.get(String.join("/", tempDirectory.toString(), fileName));

            Files.copy(multipartFile.getInputStream(), localPath);

            Path localPath2 = DockerParsePacketsUtil.parsePackets(localPath,tempDirectory);
            Jib.from(TarImage.at(localPath2))
                    .containerize(Containerizer.to(
                            RegistryImage
                                    .named(tag)
                                    .addCredential("<token>", token)
                    ).setAllowInsecureRegistries(true));

            Files.walk(tempDirectory)
                    .sorted(Comparator.reverseOrder())
                    .map(Path::toFile)
                    .forEach(File::delete);
        }catch (Exception e){
            e.printStackTrace();
            log.error("docker upload error uuid: {} ,storageId:{} ,repositoryId:{} ,tag:{}", uuid,storageId,repositoryId,tag);
        }


    }



}
