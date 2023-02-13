package com.veadan.folib.promotion;

import cn.hutool.core.io.FileUtil;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.providers.io.RepositoryPathResolver;
import com.veadan.folib.providers.layout.LayoutProvider;
import com.veadan.folib.providers.layout.LayoutProviderRegistry;
import com.veadan.folib.providers.layout.Maven2LayoutProvider;
import com.veadan.folib.services.ArtifactManagementService;
import com.veadan.folib.services.ArtifactMetadataService;
import com.veadan.folib.services.RepositoryManagementService;
import com.veadan.folib.storage.metadata.MetadataHelper;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.io.FilenameUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.apache.maven.artifact.ArtifactUtils;
import org.apache.maven.artifact.repository.metadata.Metadata;
import org.apache.maven.artifact.repository.metadata.Snapshot;
import org.apache.maven.artifact.repository.metadata.Versioning;
import org.springframework.web.multipart.MultipartFile;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.StringReader;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Calendar;
import java.util.Objects;
import java.util.Properties;
import java.util.UUID;
import java.util.concurrent.Callable;

@Slf4j
public class ArtifactUploadTask implements Callable<String> {

    private String storageId;
    private String repositoryId;
    private MultipartFile file;
    private RepositoryManagementService repositoryManagementService;
    private RepositoryPathResolver repositoryPathResolver;
    private ArtifactManagementService artifactManagementService;
    private String fileRelativePath;
    private String metaData;
    private PromotionUtil promotionUtil;
    private LayoutProviderRegistry layoutProviderRegistry;
    private ArtifactMetadataService artifactMetadataService;
    private String tempPath;

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
                              String tempPath,
                              String fileRelativePath, String metaData) {
        this.storageId = storageId;
        this.repositoryId = repositoryId;
        this.file = file;
        this.repositoryManagementService = repositoryManagementService;
        this.repositoryPathResolver = repositoryPathResolver;
        this.artifactManagementService = artifactManagementService;
        this.promotionUtil = promotionUtil;
        this.layoutProviderRegistry = layoutProviderRegistry;
        this.artifactMetadataService = artifactMetadataService;
        this.tempPath = tempPath;
        this.fileRelativePath = fileRelativePath;
        this.metaData = metaData;
    }

    @Override
    public String call() {
        String rs = "";
        try (InputStream is = file.getInputStream()) {
            RepositoryPath repositoryPath = repositoryPathResolver.resolve(storageId, repositoryId, fileRelativePath);
            String layout = repositoryPath.getRepository().getLayout();
            if (Maven2LayoutProvider.ALIAS.equals(layout)) {
                handlerMavenLayoutUpload(is, layout, repositoryPath);
            } else {
                artifactManagementService.store(repositoryPath, is);
                promotionUtil.setMetaData(repositoryPath, metaData);
            }
        } catch (IOException e) {
            log.info("store file：{}，error：{}", fileRelativePath, ExceptionUtils.getStackTrace(e));
            rs = e.getMessage();
        }
        return rs;
    }

    /**
     * 处理maven布局制品上传
     *
     * @param is             is
     * @param layout         layout
     * @param repositoryPath repositoryPath
     */
    private void handlerMavenLayoutUpload(InputStream is, String layout, RepositoryPath repositoryPath) {
        File parentTempFile = null;
        try {
            String point = ".";
            //maven布局
            parentTempFile = new File(tempPath + File.separator + UUID.randomUUID() + File.separator);
            File artifactTempFile = new File(parentTempFile.getAbsolutePath() + File.separator + fileRelativePath);
            Path path = Path.of(artifactTempFile.getAbsolutePath());
            FileUtil.writeFromStream(is, artifactTempFile);
            LayoutProvider layoutProvider = layoutProviderRegistry.getProvider(layout);
            if (Objects.nonNull(layoutProvider)) {
                String properties = layoutProvider.getContentByFileName(repositoryPath, path, "pom.properties");
                String groupId = parseProperties(properties, "groupId");
                if (groupId.contains(point)) {
                    groupId = groupId.replace(point, File.separator);
                }
                String artifactId = parseProperties(properties, "artifactId");
                if (artifactId.contains(point)) {
                    artifactId = artifactId.replace(point, File.separator);
                }
                String version = parseProperties(properties, "version");
                fileRelativePath = calcLatestSnapshotVersion(storageId, repositoryId, groupId, artifactId, version, fileRelativePath);

                String artifactPath = String.format("%s/%s/%s/%s", groupId, artifactId, version, fileRelativePath);
                log.info("maven2 layout artifact path ：{}，properties：{}，groupId：{}，artifactId：{}, version：{} artifactPath：{}", path, properties, groupId, artifactId, version, artifactPath);
                RepositoryPath artifactRepositoryPath = repositoryPathResolver.resolve(storageId, repositoryId, artifactPath);
                try (InputStream artifactInputStream = FileUtil.getInputStream(artifactTempFile)) {
                    artifactManagementService.validateAndStore(artifactRepositoryPath, artifactInputStream);
                } catch (Exception ex) {
                    log.error("store artifact：{}，error：{}", artifactRepositoryPath.toAbsolutePath(), ExceptionUtils.getStackTrace(ex));
                    throw new RuntimeException(ex.getMessage());
                }
                promotionUtil.setMetaData(artifactRepositoryPath, metaData);

                String pom = layoutProvider.getContentByFileName(repositoryPath, path, "pom.xml");
                String artifactName = fileRelativePath;
                String extension = FilenameUtils.getExtension(artifactName);
                String pomName = artifactName.replace(extension, "") + "pom";
                File pomTempFile = new File(parentTempFile.getAbsolutePath() + File.separator + pomName);
                FileUtil.writeBytes(pom.getBytes(), pomTempFile);
                String pomPath = String.format("%s/%s/%s/%s", groupId, artifactId, version, pomName);
                log.info("maven2 layout xml path ：{}，properties：{}，groupId：{}，artifactId：{}, version：{} artifactPath：{}", pomTempFile.getAbsolutePath(), properties, groupId, artifactId, version, pomPath);
                RepositoryPath pomRepositoryPath = repositoryPathResolver.resolve(storageId, repositoryId, pomPath);
                try (InputStream pomInputStream = FileUtil.getInputStream(pomTempFile)) {
                    artifactManagementService.validateAndStore(pomRepositoryPath, pomInputStream);
                } catch (Exception ex) {
                    log.error("store pom：{}，error：{}", pomRepositoryPath.toAbsolutePath(), ExceptionUtils.getStackTrace(ex));
                    throw new RuntimeException(ex.getMessage());
                }
                try {
                    String artifactIdPath = String.format("%s/%s", groupId, artifactId);
                    if (ArtifactUtils.isSnapshot(version)) {
                        artifactMetadataService.addTimestampedSnapshotVersion(storageId, repositoryId, artifactIdPath, version, null, extension);
                    }
                    artifactMetadataService.rebuildMetadata(storageId, repositoryId, artifactPath);
                } catch (Exception ex) {
                    log.error("rebuildMetadata path：{}，error：{}", artifactRepositoryPath.toAbsolutePath(), ExceptionUtils.getStackTrace(ex));
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
                metadata = artifactMetadataService.getMetadata(Files.newInputStream(metadataPath));
            }
            return metadata;
        } catch (Exception ex) {
            log.error("path：{}，getMetadata error：{}", metadataPath, ExceptionUtils.getStackTrace(ex));
            throw new RuntimeException("path：【" + metadataPath + "】getMetadata error");
        }
    }

}