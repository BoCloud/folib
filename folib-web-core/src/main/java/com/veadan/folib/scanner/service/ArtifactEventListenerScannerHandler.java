package com.veadan.folib.scanner.service;

import cn.hutool.core.io.FileTypeUtil;
import cn.hutool.core.io.FileUtil;
import cn.hutool.core.io.IoUtil;
import com.alibaba.fastjson.JSON;
import com.veadan.folib.event.AsyncEventListener;
import com.veadan.folib.event.artifact.ArtifactEvent;
import com.veadan.folib.event.artifact.ArtifactEventTypeEnum;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.providers.layout.DockerFileSystem;
import com.veadan.folib.scanner.common.constant.ScanConstans;
import com.veadan.folib.schema2.ImageManifest;
import com.veadan.folib.schema2.LayerManifest;
import com.veadan.folib.utils.ArtifactUtils;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.compress.archivers.tar.TarArchiveEntry;
import org.apache.commons.compress.archivers.tar.TarArchiveInputStream;
import org.apache.commons.compress.compressors.gzip.GzipCompressorInputStream;
import org.apache.commons.compress.utils.IOUtils;
import org.apache.commons.compress.utils.Lists;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.util.Arrays;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

/**
 * @author leipenghui
 * 制品事件监听处理
 */
@Slf4j
@Component
public class ArtifactEventListenerScannerHandler {

    @Autowired
    private ScanService scanService;

    @AsyncEventListener
    protected void handle(final ArtifactEvent<RepositoryPath> event) throws IOException {
        RepositoryPath repositoryPath = event.getPath();
        int source = (int) event.getSource();
        boolean flag = check(event);
        if (!flag) {
            return;
        }
        if (repositoryPath.getFileSystem() instanceof DockerFileSystem) {
            //docker布局
            boolean isReadFile = ArtifactEventTypeEnum.EVENT_ARTIFACT_PATH_DELETED.getType() != source && ArtifactEventTypeEnum.EVENT_ARTIFACT_DIRECTORY_PATH_DELETED.getType() != source;
            if (isReadFile) {
                String path = repositoryPath.toAbsolutePath().toString();
                File file = FileUtil.file(path);
                //版本目录
                File parentFile = file.getParentFile();
                //获取图层中的digest列表
                String manifestString = FileUtil.readString(path, StandardCharsets.UTF_8);
                ImageManifest manifest = JSON.parseObject(manifestString, ImageManifest.class);
                List<String> digestList = manifest.getLayers().stream().map(LayerManifest::getDigest).collect(Collectors.toList());
                //存放解压文件的目录路径
                String tempPath = parentFile.getPath() + File.separator + "temp";
                if (CollectionUtils.isNotEmpty(digestList)) {
                    String blobsPath = "";
                    for (String digest : digestList) {
                        blobsPath = parentFile.getParent() + File.separator + "blobs" + File.separator + digest;
                        handlerDockerBlobFile(repositoryPath, source, blobsPath, tempPath);
                    }
                }
            } else {
                handlerScan(repositoryPath, source, "", "");
            }
        } else {
            //非docker布局
            handlerScan(repositoryPath, source, "", "");
        }
    }

    /**
     * 处理docker文件
     *
     * @param repositoryPath 制品信息
     * @param source         事件类型
     * @param blobsPath      文件路径
     * @param tempPath       存放解压文件的目录路径
     */
    private void handlerDockerBlobFile(RepositoryPath repositoryPath, int source, String blobsPath, String tempPath) {
        File file = new File(blobsPath);
        //增加魔数类型
        FileTypeUtil.putFileType("1f8b08000000000000ff", "gz");
        try {
            String hex = IoUtil.readHex28Lower(new FileInputStream(file));
            log.debug("=====>>>>> 路径：{}，hex：{}", file.getName(), hex);
        } catch (Exception ex) {
            log.error("=====>>>>>读取魔数类型失败：{}", ExceptionUtils.getStackTrace(ex));
        }
        String type = FileTypeUtil.getType(file);
        String gz = "gz";
        if (gz.equals(type)) {
            log.debug("=====>>>>> 路径：{}，类型：{}", file.getName(), type);
            List<String> filePathList = readTarFile(file, tempPath);
            if (CollectionUtils.isNotEmpty(filePathList)) {
                filePathList.forEach(filePath -> {
                    handlerScan(repositoryPath, source, filePath, blobsPath);
                });
            }
        }
    }

    /**
     * 处理扫描逻辑
     *
     * @param repositoryPath 制品信息
     * @param source         事件类型
     * @param filePath       文件路径
     * @param artifactPath   制品路径
     */
    private void handlerScan(RepositoryPath repositoryPath, int source, String filePath, String artifactPath) {
        if (ArtifactEventTypeEnum.EVENT_ARTIFACT_PATH_DELETED.getType() == source) {
            scanService.checkScan(repositoryPath, ScanConstans.DEL, filePath, artifactPath);
        } else if (ArtifactEventTypeEnum.EVENT_ARTIFACT_FILE_STORED.getType() == source) {
            scanService.checkScan(repositoryPath, ScanConstans.ADD, filePath, artifactPath);
        } else if (ArtifactEventTypeEnum.EVENT_ARTIFACT_DIRECTORY_PATH_DELETED.getType() == source) {
            scanService.checkScan(repositoryPath, ScanConstans.DEL_DIRECTORY, filePath, artifactPath);
        } else {
            scanService.checkScan(repositoryPath, null, filePath, artifactPath);
        }
    }

    /**
     * 入参校验
     *
     * @param event 制品事件
     * @return true 校验通过 false 校验不通过
     */
    private boolean check(ArtifactEvent<RepositoryPath> event) {
        boolean flag = false;
        int source = (int) event.getSource();
        RepositoryPath repositoryPath = event.getPath();
        log.info("=====>>>>> 监听制品事件：{}，path路径：{}", ArtifactEventTypeEnum.queryArtifactEventTypeEnumByType(source), repositoryPath);
        ArtifactEventTypeEnum artifactEventTypeEnum = ArtifactEventTypeEnum.queryArtifactEventTypeEnumByType(source);
        if (Objects.isNull(artifactEventTypeEnum)) {
            return false;
        }
        flag = checkArtifactEvent(artifactEventTypeEnum);
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
    private boolean checkArtifactEvent(ArtifactEventTypeEnum artifactEventTypeEnum) {
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
                    break;
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
