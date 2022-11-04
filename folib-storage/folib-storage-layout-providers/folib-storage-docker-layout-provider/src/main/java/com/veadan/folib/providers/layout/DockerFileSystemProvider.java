package com.veadan.folib.providers.layout;

import cn.hutool.core.io.FileUtil;
import com.alibaba.fastjson.JSON;
import com.google.common.collect.Lists;
import com.veadan.folib.cloud.storage.s3fs.S3Iterator;
import com.veadan.folib.cloud.storage.s3fs.S3Path;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.providers.io.RepositoryPathResolver;
import com.veadan.folib.schema2.ImageManifest;
import com.veadan.folib.schema2.LayerManifest;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Value;

import javax.inject.Inject;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.spi.FileSystemProvider;
import java.util.List;
import java.util.Objects;
import java.util.UUID;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.stream.Collectors;

/**
 * @author Veadan
 */
public class DockerFileSystemProvider
        extends LayoutFileSystemProvider {

    private static final Logger logger = LoggerFactory.getLogger(DockerFileSystemProvider.class);

    @Inject
    private RepositoryPathResolver repositoryPathResolver;

    @Inject
    private DockerLayoutProvider layoutProvider;

    @Value("${folib.temp}")
    private String tempPath;

    public DockerFileSystemProvider(FileSystemProvider storageFileSystemProvider) {
        super(storageFileSystemProvider);
    }

    @Override
    protected AbstractLayoutProvider getLayoutProvider() {
        return layoutProvider;
    }

    @Override
    public void delete(Path path,
                       boolean force)
            throws IOException {
        RepositoryPath repositoryPath = (RepositoryPath) path;

        logger.debug("Removing {}...", repositoryPath);
        handlerManifestAndBlob(repositoryPath, force);
        super.delete(repositoryPath, force);
    }

    /**
     * 处理manifest和blob文件
     *
     * @param repositoryPath 路径
     * @param force          是否强制
     * @throws IOException io异常
     */
    private void handlerManifestAndBlob(RepositoryPath repositoryPath, boolean force) throws IOException {
        if (!Files.isDirectory(repositoryPath)) {
            return;
        }
        //当前版本下manifest文件信息
        Path currentManifestPath = null;
        List<Path> pathList = Files.list(repositoryPath).filter(f -> !Files.isDirectory(f) && !f.getFileName().toString().endsWith(".sha256") && f.getFileName().toString().startsWith("sha256")).collect(Collectors.toList());
        if (CollectionUtils.isNotEmpty(pathList)) {
            currentManifestPath = pathList.get(0);
        }
        if (Objects.nonNull(currentManifestPath)) {
            RepositoryPath parent = repositoryPath.getParent();
            String manifest = "manifest";
            logger.debug("=====>>>>>当前版本下的manifest路径：{}", currentManifestPath.toAbsolutePath().toString());
            //manifest目录下的当前版本的文件信息
            RepositoryPath manifestRepositoryPath = repositoryPathResolver.resolve(repositoryPath.getStorageId(), repositoryPath.getRepositoryId(), parent.getFileName() + File.separator + manifest + File.separator + currentManifestPath.getFileName().toString());
            logger.debug("=====>>>>>manifest目录下的当前版本的文件路径：{}", manifestRepositoryPath.toAbsolutePath());
            boolean isS3 = repositoryPath.getTarget() instanceof S3Path;
            boolean flag;
            if (isS3) {
                S3Path s3Path = (S3Path) repositoryPath.getTarget();
                flag = checkS3DockerRelation(s3Path, manifestRepositoryPath.getFileName().toString(), repositoryPath.getFileName().toString());
            } else {
                flag = checkRelation(parent.toAbsolutePath().toString(), manifestRepositoryPath.getFileName().toString(), repositoryPath.getFileName().toString());
            }
            if (!flag) {
                //当前版本下的manifest信息
                if (isS3) {
                    handlerS3Path(currentManifestPath, parent, manifestRepositoryPath, repositoryPath, force);
                } else {
                    handlerLocalPath(currentManifestPath, parent, manifestRepositoryPath, repositoryPath, force);
                }
            }
        }
    }

    private void handlerLocalPath(Path currentManifestPath, RepositoryPath parent, RepositoryPath manifestRepositoryPath, RepositoryPath repositoryPath, boolean force) throws IOException {
        String manifest = "manifest";
        String blobs = "blobs";
        String currentManifestString = FileUtil.readString(repositoryPath.toAbsolutePath() + File.separator + currentManifestPath.getFileName().toString(), StandardCharsets.UTF_8);
        ImageManifest currentManifest = JSON.parseObject(currentManifestString, ImageManifest.class);
        //当前版本下的层级信息
        List<LayerManifest> currentLayerManifestList = currentManifest.getLayers();
        //存放在其他版本下使用到的层级信息
        List<LayerManifest> layerManifestExistList = Lists.newArrayList();
        //docker镜像下的manifest目录路径
        String manifestPath = parent.toAbsolutePath().toString() + File.separator + manifest;
        //docker镜像下的manifest目录信息
        Path path = Path.of(manifestPath);
        //过滤找出其他版本的manifest文件信息
        Path finalCurrentManifestPath = currentManifestPath;
        List<String> manifestConfigList = Lists.newArrayList();
        Files.list(path).filter(f -> !Files.isDirectory(f) && f.getFileName().toString().startsWith("sha256") && !f.getFileName().toString().endsWith(".sha256") && !f.getFileName().toString().equals(finalCurrentManifestPath.getFileName().toString())).forEach(f -> {
            logger.debug("=====>>>>>其他版本的manifest文件名：{}", f.getFileName().toString());
            String manifestStringItem = FileUtil.readString(manifestPath + File.separator + f.getFileName().toString(), StandardCharsets.UTF_8);
            ImageManifest manifestItem = JSON.parseObject(manifestStringItem, ImageManifest.class);
            manifestConfigList.add(manifestItem.getConfig().getDigest());
            //循环查询当前版本下的被其他版本使用了的层级信息
            for (LayerManifest layerManifest : currentLayerManifestList) {
                if (layersContains(layerManifest, manifestItem.getLayers())) {
                    layerManifestExistList.add(layerManifest);
                }
            }
        });
        currentLayerManifestList.removeAll(layerManifestExistList);
        //配置信息
        String manifestString = FileUtil.readString(manifestRepositoryPath.toAbsolutePath().toString(), StandardCharsets.UTF_8);
        ImageManifest imageManifest = JSON.parseObject(manifestString, ImageManifest.class);
        String configDigest = imageManifest.getConfig().getDigest();
        //不存在关联，删除manifest目录下的当前版本信息
        this.delete(manifestRepositoryPath, force);
        //当前版本下的配置信息
        String currentConfigDigest = currentManifest.getConfig().getDigest();
        if (!manifestConfigList.contains(currentConfigDigest)) {
            //删除blobs目录下的配置信息
            RepositoryPath configRepositoryPath = repositoryPathResolver.resolve(repositoryPath.getStorageId(), repositoryPath.getRepositoryId(), parent.getFileName() + File.separator + blobs + File.separator + configDigest);
            logger.debug("=====>>>>>configRepositoryPath：{}", configRepositoryPath.toAbsolutePath());
            this.delete(configRepositoryPath, force);
        }
        //删除blobs目录下的层级文件
        if (CollectionUtils.isNotEmpty(currentLayerManifestList)) {
            for (LayerManifest item : currentLayerManifestList) {
                String blobRepositoryPath = blobs + File.separator + item.getDigest();
                RepositoryPath itemRepositoryPath = repositoryPathResolver.resolve(repositoryPath.getStorageId(), repositoryPath.getRepositoryId(), parent.getFileName() + File.separator + blobRepositoryPath);
                logger.debug("=====>>>>>blobRepositoryPath：{}", itemRepositoryPath.toAbsolutePath());
                this.delete(itemRepositoryPath, force);
            }
        }
    }

    private void handlerS3Path(Path currentManifestPath, RepositoryPath parent, RepositoryPath manifestRepositoryPath, RepositoryPath repositoryPath, boolean force) {
        //S3存储
        String parentPath = "";
        try {
            String manifest = "manifest";
            String blobs = "blobs";
            S3Path s3Path = (S3Path) repositoryPath.getTarget();
            InputStream inputStream = Files.newInputStream(manifestRepositoryPath);
            parentPath = tempPath + File.separator + UUID.randomUUID();
            String filePath = parentPath + File.separator + s3Path.getFileName();
            File tempFile = new File(filePath);
            FileUtil.writeFromStream(inputStream, tempFile, true);
            //获取图层中的digest列表
            String currentManifestString = FileUtil.readString(tempFile.getAbsolutePath(), StandardCharsets.UTF_8);
            ImageManifest currentManifest = JSON.parseObject(currentManifestString, ImageManifest.class);
            //当前版本下的层级信息
            List<LayerManifest> currentLayerManifestList = currentManifest.getLayers();
            //存放在其他版本下使用到的层级信息
            List<LayerManifest> layerManifestExistList = Lists.newArrayList();
            //过滤找出其他版本的manifest文件信息
            List<String> manifestConfigList = Lists.newArrayList();
            Path path = s3Path.getParent();
            S3Path parentS3Path = (S3Path) path;
            S3Iterator s3Iterator = new S3Iterator(parentS3Path);
            while (s3Iterator.hasNext()) {
                S3Path childrenS3Path = s3Iterator.next();
                if (manifest.equals(childrenS3Path.getFileName().toString())) {
                    S3Iterator manifestS3Iterator = new S3Iterator(childrenS3Path);
                    while (manifestS3Iterator.hasNext()) {
                        S3Path itemManifestPath = manifestS3Iterator.next();
                        if (itemManifestPath.getFileName().toString().startsWith("sha256") && !itemManifestPath.getFileName().toString().endsWith(".sha256") && !itemManifestPath.getFileName().toString().equals(currentManifestPath.getFileName().toString())) {
                            logger.debug("=====>>>>>其他版本的manifest文件名：{}", itemManifestPath.getFileName().toString());
                            inputStream = Files.newInputStream(itemManifestPath);
                            filePath = parentPath + File.separator + itemManifestPath.getFileName();
                            tempFile = new File(filePath);
                            FileUtil.writeFromStream(inputStream, tempFile, true);
                            String manifestStringItem = FileUtil.readString(tempFile, StandardCharsets.UTF_8);
                            ImageManifest manifestItem = JSON.parseObject(manifestStringItem, ImageManifest.class);
                            manifestConfigList.add(manifestItem.getConfig().getDigest());
                            //循环查询当前版本下的被其他版本使用了的层级信息
                            for (LayerManifest layerManifest : currentLayerManifestList) {
                                if (layersContains(layerManifest, manifestItem.getLayers())) {
                                    layerManifestExistList.add(layerManifest);
                                }
                            }
                        }
                    }
                }
            }
            currentLayerManifestList.removeAll(layerManifestExistList);
            //配置信息
            inputStream = Files.newInputStream(manifestRepositoryPath);
            filePath = parentPath + File.separator + manifestRepositoryPath.getFileName();
            tempFile = new File(filePath);
            FileUtil.writeFromStream(inputStream, tempFile, true);
            String manifestString = FileUtil.readString(tempFile, StandardCharsets.UTF_8);
            ImageManifest imageManifest = JSON.parseObject(manifestString, ImageManifest.class);
            String configDigest = imageManifest.getConfig().getDigest();
            //不存在关联，删除manifest目录下的当前版本信息
            this.delete(manifestRepositoryPath, force);
            //当前版本下的配置信息
            String currentConfigDigest = currentManifest.getConfig().getDigest();
            if (!manifestConfigList.contains(currentConfigDigest)) {
                //删除blobs目录下的配置信息
                RepositoryPath configRepositoryPath = repositoryPathResolver.resolve(repositoryPath.getStorageId(), repositoryPath.getRepositoryId(), parent.getFileName() + File.separator + blobs + File.separator + configDigest);
                logger.debug("=====>>>>>configRepositoryPath：{}", configRepositoryPath.toAbsolutePath());
                this.delete(configRepositoryPath, force);
            }
            //删除blobs目录下的层级文件
            if (CollectionUtils.isNotEmpty(currentLayerManifestList)) {
                for (LayerManifest item : currentLayerManifestList) {
                    String blobRepositoryPath = blobs + File.separator + item.getDigest();
                    RepositoryPath itemRepositoryPath = repositoryPathResolver.resolve(repositoryPath.getStorageId(), repositoryPath.getRepositoryId(), parent.getFileName() + File.separator + blobRepositoryPath);
                    logger.debug("=====>>>>>blobRepositoryPath：{}", itemRepositoryPath.toAbsolutePath());
                    this.delete(itemRepositoryPath, force);
                }
            }
        } catch (Exception ex) {
            logger.error("=====>>>>>获取S3 docker 层级信息错误：{}", ExceptionUtils.getStackTrace(ex));
            throw new RuntimeException(ex.getMessage());
        } finally {
            //删除临时文件
            if (StringUtils.isNotBlank(parentPath)) {
                FileUtil.del(new File(parentPath));
            }
        }
    }

    /**
     * 校验当前版本的manifest文件是否被其他版本使用
     *
     * @param parentPath    父级目录
     * @param fileName      文件名称
     * @param directoryName 当前目录名称
     * @return true 在使用 false 不在使用
     */
    private boolean checkRelation(String parentPath, String fileName, String directoryName) {
        //镜像顶级路径
        Path path = Path.of(parentPath);
        AtomicBoolean flag = new AtomicBoolean(false);
        List<String> directoryNameList = Lists.newArrayList("blobs", "manifest", directoryName);
        try {
            Files.list(path).filter(f -> Files.isDirectory(f) && !directoryNameList.contains(f.getFileName().toString())).forEach(item -> {
                try {
                    boolean b = Files.list(item).anyMatch(fc -> !Files.isDirectory(fc) && fc.getFileName().toString().equals(fileName));
                    if (b) {
                        logger.debug("=====>>>>>存在关联，版本目录名称：{}", item.getFileName().toString());
                        flag.set(true);
                    }
                } catch (IOException ex) {
                    logger.error("=====>>>>>IO异常：{}", ExceptionUtils.getStackTrace(ex));
                }
            });
        } catch (IOException ex) {
            logger.error("=====>>>>>IO异常：{}", ExceptionUtils.getStackTrace(ex));
        }
        return flag.get();
    }

    /**
     * 校验当前版本的manifest文件是否被其他版本使用
     *
     * @param s3Path        父级目录
     * @param fileName      文件名称
     * @param directoryName 当前目录名称
     * @return true 在使用 false 不在使用
     */
    private boolean checkS3DockerRelation(S3Path s3Path, String fileName, String directoryName) {
        //镜像顶级路径
        Path parentPath = s3Path.getParent();
        S3Path parentS3Path = (S3Path) parentPath;
        S3Iterator s3Iterator = new S3Iterator(parentS3Path);
        AtomicBoolean flag = new AtomicBoolean(false);
        List<String> directoryNameList = Lists.newArrayList("blobs", "manifest", directoryName);
        while (s3Iterator.hasNext()) {
            if (flag.get()) {
                break;
            }
            S3Path sonS3Path = s3Iterator.next();
            if (!directoryNameList.contains(sonS3Path.getFileName().toString())) {
                S3Iterator sonS3Iterator = new S3Iterator(sonS3Path);
                while (sonS3Iterator.hasNext()) {
                    S3Path grandsonS3Path = sonS3Iterator.next();
                    if (fileName.equals(grandsonS3Path.getFileName().toString())) {
                        logger.debug("=====>>>>>存在关联，版本目录名称：{}", grandsonS3Path.getFileName().toString());
                        flag.set(true);
                        break;
                    }
                }
            }
        }
        return flag.get();
    }

    /**
     * 判断layer是否包含
     *
     * @param layerManifest     layerManifest
     * @param layerManifestList layerManifestList
     * @return true 包含 false 不包含
     */
    private boolean layersContains(LayerManifest layerManifest, List<LayerManifest> layerManifestList) {
        if (CollectionUtils.isEmpty(layerManifestList)) {
            return false;
        }
        long count = layerManifestList.stream().filter(item -> item.getDigest().equals(layerManifest.getDigest()) && item.getMediaType().equals(layerManifest.getMediaType())).count();
        return count > 0L;
    }


}
