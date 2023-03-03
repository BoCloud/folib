package com.veadan.folib.scanner.service;

import cn.hutool.core.io.FileTypeUtil;
import cn.hutool.core.io.FileUtil;
import cn.hutool.core.io.IoUtil;
import com.alibaba.fastjson.JSON;
import com.google.common.collect.Sets;
import com.veadan.folib.cloud.storage.s3fs.S3Path;
import com.veadan.folib.domain.Artifact;
import com.veadan.folib.enums.SafeLevelEnum;
import com.veadan.folib.event.AsyncEventListener;
import com.veadan.folib.event.artifact.ArtifactEvent;
import com.veadan.folib.event.artifact.ArtifactEventTypeEnum;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.providers.io.RepositoryPathResolver;
import com.veadan.folib.providers.layout.DockerFileSystem;
import com.veadan.folib.schema2.ImageManifest;
import com.veadan.folib.schema2.LayerManifest;
import com.veadan.folib.services.ArtifactService;
import com.veadan.folib.utils.ArtifactUtils;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.compress.archivers.tar.TarArchiveEntry;
import org.apache.commons.compress.archivers.tar.TarArchiveInputStream;
import org.apache.commons.compress.compressors.gzip.GzipCompressorInputStream;
import org.apache.commons.compress.utils.IOUtils;
import org.apache.commons.compress.utils.Lists;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;
import software.amazon.awssdk.services.s3.model.PutObjectRequest;

import javax.inject.Inject;
import java.io.*;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.*;
import java.util.stream.Collectors;

/**
 * @author leipenghui
 * 制品事件监听处理
 */
@Slf4j
@Component
public class ArtifactEventListenerScannerHandler {

    @Inject
    private ArtifactService artifactService;

    @Inject
    protected RepositoryPathResolver repositoryPathResolver;

    @Value("${folib.temp}")
    private String tempPath;

    /**
     * 对接devops 平台查询docker 包详情返回下载地址。配置的路径下相关应用包路径
     */
    private static List<String> extract = Arrays.asList(System.getProperty("folib.docker.getAppPackagePathConfig", "usr/local/app").split(","));

    @AsyncEventListener
    protected void handle(final ArtifactEvent<RepositoryPath> event) throws IOException {
        RepositoryPath repositoryPath = event.getPath();
        int source = (int) event.getSource();
        boolean validate = validateEventTypeAndLayoutSupports(event);
        if (!validate) {
            return;
        }
        log.info("=====>>>>> start handler artifact event：{}，path：{}", ArtifactEventTypeEnum.queryArtifactEventTypeEnumByType(source), repositoryPath);
        if (repositoryPath.getFileSystem() instanceof DockerFileSystem) {
            //docker布局
            boolean isStoredEvent = ArtifactEventTypeEnum.EVENT_ARTIFACT_PATH_DELETED.getType() != source && ArtifactEventTypeEnum.EVENT_ARTIFACT_DIRECTORY_PATH_DELETED.getType() != source;
            if (isStoredEvent) {
                Path path = repositoryPath.getTarget();
                if (path instanceof S3Path) {
                    //S3存储
                    handlerDockerS3Path(repositoryPath, path, source);
                } else {
                    handlerDockerFilePath(repositoryPath, source);
                }
            } else {
                handlerScan(repositoryPath, source);
            }
        } else {
            //非docker布局
            handlerScan(repositoryPath, source);
        }
    }

    /**
     * 处理S3存储docker布局制品事件
     *
     * @param repositoryPath 制品路径
     * @param path           path
     * @param source         事件类型
     */
    private void handlerDockerS3Path(RepositoryPath repositoryPath, Path path, Integer source) {
        String parentPath = "";
        try {
            //S3存储
            S3Path s3Path = (S3Path) path;
            String key = s3Path.getKey();
            String versionKey = key.substring(0, key.lastIndexOf("/"));
            InputStream inputStream = Files.newInputStream(repositoryPath);
            parentPath = tempPath + File.separator + UUID.randomUUID();
            String filePath = parentPath + File.separator + s3Path.getFileName();
            File tempFile = new File(filePath);
            FileUtil.writeFromStream(inputStream, tempFile, true);
            //获取图层中的digest列表
            List<String> digestList = getImageManifest(tempFile);
            if (CollectionUtils.isNotEmpty(digestList)) {
                String prefix = versionKey;
                prefix = prefix.substring(0, prefix.lastIndexOf("/"));
                String blobsPath = "", tempPath = "";
                Set<String> filePaths = Sets.newLinkedHashSet();
                for (String digest : digestList) {
                    blobsPath = prefix + File.separator + "blobs" + File.separator + digest;
                    String blobsItemPath = blobsPath.replace(String.format("%s/", repositoryPath.getRepositoryId()), "");
                    RepositoryPath blobsRepositoryPath = repositoryPathResolver.resolve(repositoryPath.getStorageId(), repositoryPath.getRepositoryId(), blobsItemPath);
                    filePath = parentPath + File.separator + digest;
                    tempFile = new File(filePath);
                    inputStream = Files.newInputStream(blobsRepositoryPath);
                    FileUtil.writeFromStream(inputStream, tempFile, true);
                    tempPath = parentPath + File.separator + "temp";
                    handlerDockerBlobFile(repositoryPath, filePaths, tempFile.getPath(), tempPath);
                }
                handlerScan(repositoryPath, source, filePaths);
            }
        } catch (Exception ex) {
            log.error("=====>>>>>处理S3存储docker布局制品事件错误：{}", ExceptionUtils.getStackTrace(ex));
        } finally {
            //删除临时文件
            if (StringUtils.isNotBlank(parentPath)) {
                FileUtil.del(new File(parentPath));
            }
        }
    }


    /**
     * 处理本地或nsf存储docker布局制品事件
     *
     * @param repositoryPath 制品路径
     * @param source         事件类型
     */
    private void handlerDockerFilePath(RepositoryPath repositoryPath, Integer source) {
        String filePath = repositoryPath.toAbsolutePath().toString();
        File file = FileUtil.file(filePath);
        //版本目录
        File parentFile = file.getParentFile();
        //获取图层中的digest列表
        List<String> digestList = getImageManifest(file);
        //存放解压文件的目录路径
        String tempPath = parentFile.getPath() + File.separator + "temp";
        if (CollectionUtils.isNotEmpty(digestList)) {
            String blobsPath = "";
            Set<String> filePaths = Sets.newLinkedHashSet();
            for (String digest : digestList) {
                blobsPath = parentFile.getParent() + File.separator + "blobs" + File.separator + digest;
                handlerDockerBlobFile(repositoryPath, filePaths, blobsPath, tempPath);
            }
            handlerScan(repositoryPath, source, filePaths);
        }
    }

    private List<String> getImageManifest(File file) {
        String manifestString = FileUtil.readString(file.getAbsolutePath(), StandardCharsets.UTF_8);
        try {
            ImageManifest manifest = JSON.parseObject(manifestString, ImageManifest.class);
            return manifest.getLayers().stream().map(LayerManifest::getDigest).collect(Collectors.toList());
        } catch (Exception ex) {
            log.error("getImageManifest error file：{}，error：{}", file.getAbsolutePath(), ExceptionUtils.getStackTrace(ex));
            throw new RuntimeException(file.getAbsolutePath() + " get image manifest error");
        }
    }

    /**
     * 处理docker文件
     *
     * @param repositoryPath 制品信息
     * @param filePaths      路径集合
     * @param blobsPath      文件路径
     * @param tempPath       存放解压文件的目录路径
     */
    private void handlerDockerBlobFile(RepositoryPath repositoryPath, Set<String> filePaths, String blobsPath, String tempPath) {
        File file = new File(blobsPath);
        //增加魔数类型
        FileTypeUtil.putFileType("1f8b08000000000000ff", "gz");
        try {
            String hex = IoUtil.readHex28Lower(new FileInputStream(file));
            log.debug("=====>>>>> 路径：{}，hex：{}", file.getAbsolutePath(), hex);
        } catch (Exception ex) {
            log.error("=====>>>>>读取魔数类型失败：{}", ExceptionUtils.getStackTrace(ex));
        }
        String type = FileTypeUtil.getType(file);
        String gz = "gz";
        if (gz.equals(type)) {
            log.debug("=====>>>>> 路径：{}，类型：{}", file.getAbsolutePath(), type);
            List<String> filePathList = readTarFile(file, tempPath);
            if (CollectionUtils.isNotEmpty(filePathList)) {
                Path path = repositoryPath.getTarget();
                final boolean isS3 = (path instanceof S3Path);
                S3Path s3Path = null;
                String prefix = null, versionKey = null;
                if (isS3) {
                    s3Path = (S3Path) path;
                    String key = s3Path.getKey();
                    versionKey = key.substring(0, key.lastIndexOf("/"));
                    prefix = versionKey.substring(0, versionKey.lastIndexOf("/"));
                }
                S3Path finalS3Path = s3Path;
                String finalPrefix = prefix;
                String finalVersionKey = versionKey;
                filePathList.forEach(filePath -> {
                    if (isS3) {
                        S3Path s3PathObject = new S3Path(finalS3Path.getFileSystem(), finalVersionKey + File.separator + "temp" + File.separator + FileUtil.getName(filePath));
                        //将docker镜像中解压出来的文件上传到S3
                        finalS3Path.getFileSystem().getClient().putObject(PutObjectRequest.builder().bucket(finalS3Path.getBucketName()).key(s3PathObject.getKey()).build(), Path.of(filePath));
                        filePaths.add(finalS3Path.toString().substring(0, finalS3Path.toString().indexOf(finalS3Path.getKey())) + s3PathObject.toString());
                    } else {
                        filePaths.add(filePath);
                    }
                });
            }
        }
    }

    /**
     * 处理扫描逻辑
     *
     * @param repositoryPath 制品信息
     * @param source         事件类型
     */
    private void handlerScan(RepositoryPath repositoryPath, int source) {
        if (ArtifactEventTypeEnum.EVENT_ARTIFACT_PATH_DELETED.getType() != source && ArtifactEventTypeEnum.EVENT_ARTIFACT_DIRECTORY_PATH_DELETED.getType() != source) {
            try {
                Artifact artifact = repositoryPath.getArtifactEntry();
                if (artifact == null) {
                    log.warn("No [{}] for [{}].",
                            Artifact.class.getSimpleName(),
                            repositoryPath);

                    return;
                }
                artifact.setSafeLevel(SafeLevelEnum.UN_SCAN.getLevel());
                Set<String> filePaths = Sets.newLinkedHashSet();
                filePaths.add(repositoryPath.toAbsolutePath().toString());
                artifact.setFilePaths(filePaths);
                artifactService.saveOrUpdateArtifact(artifact);
            } catch (IOException ex) {
                log.error("=====>>>>>获取Artifact错误：{}", ExceptionUtils.getStackTrace(ex));
            }
        }
    }

    /**
     * 处理扫描逻辑
     *
     * @param repositoryPath 制品信息
     * @param source         事件类型
     * @param filePaths      文件路径集合
     */
    private void handlerScan(RepositoryPath repositoryPath, int source, Set<String> filePaths) {
        if (ArtifactEventTypeEnum.EVENT_ARTIFACT_PATH_DELETED.getType() != source && ArtifactEventTypeEnum.EVENT_ARTIFACT_DIRECTORY_PATH_DELETED.getType() != source) {
            try {
                Artifact artifact = repositoryPath.getArtifactEntry();
                if (artifact == null) {
                    log.warn("No [{}] for [{}].",
                            Artifact.class.getSimpleName(),
                            repositoryPath);

                    return;
                }
                artifact.setSafeLevel(SafeLevelEnum.UN_SCAN.getLevel());
                artifact.setFilePaths(filePaths);
                artifactService.saveOrUpdateArtifact(artifact);
            } catch (IOException ex) {
                log.error("=====>>>>>获取Artifact错误：{}", ExceptionUtils.getStackTrace(ex));
            }
        }
    }

    /**
     * 校验是否是需要处理的事件类型、布局下的制品文件
     *
     * @param event 制品事件
     * @return true 校验通过 false 校验不通过
     */
    private boolean validateEventTypeAndLayoutSupports(ArtifactEvent<RepositoryPath> event) {
        boolean flag = false;
        int source = (int) event.getSource();
        RepositoryPath repositoryPath = event.getPath();
        ArtifactEventTypeEnum artifactEventTypeEnum = ArtifactEventTypeEnum.queryArtifactEventTypeEnumByType(source);
        log.debug("=====>>>>> 监听到制品事件：{}，path路径：{}", artifactEventTypeEnum, repositoryPath);
        if (Objects.isNull(artifactEventTypeEnum)) {
            return false;
        }
        flag = validateArtifactEvent(artifactEventTypeEnum);
        log.debug("=====>>>>> 制品事件类型是否为需要处理的类型：{}", flag);
        if (ArtifactEventTypeEnum.EVENT_ARTIFACT_DIRECTORY_PATH_DELETED.getType() == source) {
            //删除制品目录后续不需要校验文件类型是否支持
            return true;
        }
        if (flag) {
            flag = ArtifactUtils.layoutSupports(repositoryPath);
        }
        return flag;
    }

    /**
     * 校验制品事件类型是否为需要处理的类型
     *
     * @param artifactEventTypeEnum 制品事件类型
     * @return true 需要处理 false 不需要处理
     */
    private boolean validateArtifactEvent(ArtifactEventTypeEnum artifactEventTypeEnum) {
        List<Integer> list = Arrays.asList(ArtifactEventTypeEnum.EVENT_ARTIFACT_FILE_STORED.getType(), ArtifactEventTypeEnum.EVENT_ARTIFACT_FILE_UPDATED.getType(), ArtifactEventTypeEnum.EVENT_ARTIFACT_PATH_DELETED.getType(), ArtifactEventTypeEnum.EVENT_ARTIFACT_DIRECTORY_PATH_DELETED.getType());
        return list.contains(artifactEventTypeEnum.getType());
    }


    /**
     * 读取tar.gz 文件
     *
     * @param tarFile  文件信息
     * @param tempPath 存放解压文件的目录路径
     * @return 文件路径列表
     */
    public static List<String> readTarFile(File tarFile, String tempPath) {
        FileInputStream fileInputStream = null;
        GzipCompressorInputStream gzipCompressorInputStream = null;
        TarArchiveInputStream tarArchiveInputStream = null;
        List<String> pathList = Lists.newArrayList();
        try {
            fileInputStream = new FileInputStream(tarFile);
            gzipCompressorInputStream = new GzipCompressorInputStream(fileInputStream);
            tarArchiveInputStream = new TarArchiveInputStream(gzipCompressorInputStream);
            TarArchiveEntry entry = null;
            List<String> list = Arrays.asList("jar", "war", "ear", "zip", "json", "tgz", "nupkg", "nuspec", "config", "whl", "egg", "zip", "gz", "rpm");
            File extractFolder = new File(tempPath);
            while ((entry = tarArchiveInputStream.getNextTarEntry()) != null) {
                if (entry.isDirectory()) {
                    continue;
                }
                if (entry.getSize() > 0) {
                    String type = FileUtil.getSuffix(entry.getName());
                    if (list.contains(type)) {
                        File curFile = new File(extractFolder, entry.getName());
                        File parent = curFile.getParentFile();
                        if (!parent.exists()) {
                            parent.mkdirs();
                        }
                        FileOutputStream fileOutputStream = new FileOutputStream(curFile);
                        IOUtils.copy(tarArchiveInputStream, fileOutputStream);
                        IOUtils.closeQuietly(fileOutputStream);
                        pathList.add(curFile.getPath());
                        log.debug("=====>>>>> 文件名称：{}，文件类型：{}，生成文件路径：{}", entry.getName(), type, curFile.getPath());
                    }
                }
            }
        } catch (Exception e) {
            log.error("=====>>>>>读取tar.gz文件失败：{}", ExceptionUtils.getStackTrace(e));
        } finally {
            try {
                if (Objects.nonNull(fileInputStream)) {
                    fileInputStream.close();
                }
                if (Objects.nonNull(gzipCompressorInputStream)) {
                    gzipCompressorInputStream.close();
                }
                if (Objects.nonNull(tarArchiveInputStream)) {
                    tarArchiveInputStream.close();
                }
            } catch (IOException ex) {
                log.error("=====>>>>>关闭IO流失败：{}", ExceptionUtils.getStackTrace(ex));
            }
        }
        return pathList;
    }

}
