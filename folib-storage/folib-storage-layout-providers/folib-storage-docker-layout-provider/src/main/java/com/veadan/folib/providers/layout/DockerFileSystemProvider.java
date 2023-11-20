package com.veadan.folib.providers.layout;

import com.alibaba.fastjson.JSON;
import com.google.common.collect.Lists;
import com.veadan.folib.artifact.coordinates.DockerArtifactCoordinates;
import com.veadan.folib.providers.io.RepositoryFiles;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.providers.io.RepositoryPathResolver;
import com.veadan.folib.schema2.ImageManifest;
import com.veadan.folib.schema2.LayerManifest;
import com.veadan.folib.schema2.Manifests;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Value;

import javax.inject.Inject;
import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.spi.FileSystemProvider;
import java.util.Collections;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.stream.Collectors;
import java.util.stream.Stream;

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
        logger.info("Removing {}...", repositoryPath);
        handlerManifestAndBlob(repositoryPath, force);
        super.delete(repositoryPath, force);
        RepositoryPath parent = repositoryPath.getParent();
        try {
            if (Files.exists(parent) && !Files.isSameFile(repositoryPath.getRoot(), parent) && Files.list(parent).count() == 0) {
                Files.deleteIfExists(parent);
                logger.info("Delete parent root path {}", parent.toString());
            }
        } catch (Exception ex) {
            logger.error("删除父目录失败 {}", ExceptionUtils.getStackTrace(ex));
        }
    }

    /**
     * 处理manifest和blob文件
     *
     * @param repositoryPath 路径
     * @param force          是否强制
     * @throws IOException io异常
     */
    public void handlerManifestAndBlob(RepositoryPath repositoryPath, boolean force) throws IOException {
        handlerManifestAndBlob(repositoryPath, force, null);
    }

    /**
     * 处理manifest和blob文件
     *
     * @param repositoryPath      路径
     * @param force               是否强制
     * @param currentManifestPath manifestPath
     * @throws IOException io异常
     */
    public void handlerManifestAndBlob(RepositoryPath repositoryPath, boolean force, Path currentManifestPath) throws IOException {
        if (!Files.isDirectory(repositoryPath)) {
            return;
        }
        if (Objects.isNull(currentManifestPath)) {
            //当前版本下manifest文件信息
            List<Path> pathList;
            try (Stream<Path> pathStream = Files.list(repositoryPath)) {
                pathList = pathStream.filter(f -> !Files.isDirectory(f) && DockerArtifactCoordinates.isManifestPath(f)).collect(Collectors.toList());
            }
            if (CollectionUtils.isNotEmpty(pathList)) {
                currentManifestPath = pathList.get(0);
            }
        }
        if (Objects.nonNull(currentManifestPath)) {
            RepositoryPath parent = repositoryPath.getParent();
            String manifest = "manifest";
            logger.info("当前版本下的manifest路径：{}", currentManifestPath.toAbsolutePath().toString());
            //manifest目录下的当前版本的文件信息
            RepositoryPath manifestRepositoryPath = repositoryPathResolver.resolve(repositoryPath.getStorageId(), repositoryPath.getRepositoryId(), parent.getFileName() + File.separator + manifest + File.separator + currentManifestPath.getFileName().toString());
            logger.info("manifest目录下的当前版本的文件路径：{}", manifestRepositoryPath.toAbsolutePath());
            boolean flag = checkRelation(parent, manifestRepositoryPath.getFileName().toString(), repositoryPath.getFileName().toString());
            if (!flag) {
                handlerLocalPath(currentManifestPath, parent, manifestRepositoryPath, repositoryPath, force);
            }
        }
    }

    public void handlerLocalPath(Path currentManifestPath, RepositoryPath parent, RepositoryPath manifestRepositoryPath, RepositoryPath repositoryPath, boolean force) throws IOException {
        String manifest = "manifest";
        String blobs = "blobs";
        List<ImageManifest> currentManifestList = getImageManifests(manifestRepositoryPath);
        if (CollectionUtils.isEmpty(currentManifestList)) {
            return;
        }
        List<String> imageManifestDigestList = currentManifestList.stream().filter(item -> StringUtils.isNotBlank(item.getDigest())).map(ImageManifest::getDigest).collect(Collectors.toList());
        for (ImageManifest itemImageManifest : currentManifestList) {
            RepositoryPath manifestRootRepositoryPath = parent.resolve(manifest);
            //当前版本下的层级信息
            List<LayerManifest> currentLayerManifestList = itemImageManifest.getLayers();
            if (CollectionUtils.isEmpty(currentLayerManifestList)) {
                logger.info("Delete manifestRepositoryPath：{}", manifestRepositoryPath.toAbsolutePath());
                this.delete(manifestRepositoryPath, force);
                continue;
            }
            //存放在其他版本下使用到的层级信息
            List<LayerManifest> layerManifestExistList = Lists.newArrayList();
            //过滤找出其他版本的manifest文件信息
            List<String> manifestConfigList = Lists.newArrayList();
            try (Stream<Path> pathStream = Files.list(manifestRootRepositoryPath)) {
                pathStream.filter(f -> !Files.isDirectory(f) && DockerArtifactCoordinates.isManifestPath(f) && !f.getFileName().toString().equals(currentManifestPath.getFileName().toString()) && imageManifestDigestList.stream().noneMatch(f.getFileName().toString()::equals)).forEach(f -> {
                    logger.info("其他版本的manifest文件名：{}", f.getFileName().toString());
                    RepositoryPath itemManifestRepositoryPath = parent.resolve(manifest + File.separator + f.getFileName().toString());
                    try {
                        List<ImageManifest> imageManifestList = getImageManifests(itemManifestRepositoryPath);
                        manifestConfigList.addAll(Optional.ofNullable(imageManifestList).orElse(Collections.emptyList()).stream().filter(item -> Objects.nonNull(item.getConfig())).map(item -> item.getConfig().getDigest()).collect(Collectors.toList()));
                        List<LayerManifest> layerManifests = Optional.ofNullable(imageManifestList).orElse(Collections.emptyList()).stream().flatMap(ele -> Optional.ofNullable(ele.getLayers()).orElse(Collections.emptyList()).stream()).collect(Collectors.toList());
                        //循环查询当前版本下的被其他版本使用了的层级信息
                        for (LayerManifest layerManifest : currentLayerManifestList) {
                            if (layersContains(layerManifest, layerManifests)) {
                                layerManifestExistList.add(layerManifest);
                            }
                        }
                    } catch (IOException ex) {
                        logger.error("获取 {} 的imageManifestList错误 {}", itemManifestRepositoryPath, ExceptionUtils.getStackTrace(ex));
                    }
                });
            }
            currentLayerManifestList.removeAll(layerManifestExistList);
            RepositoryPath currentManifestRepositoryPath = repositoryPathResolver.resolve(repositoryPath.getStorageId(), repositoryPath.getRepositoryId(), parent.getFileName() + File.separator + manifest + File.separator + itemImageManifest.getDigest());
            boolean flag = checkRelation(parent, currentManifestRepositoryPath.getFileName().toString(), repositoryPath.getFileName().toString());
            if (!flag) {
                //配置信息，不存在关联，删除manifest目录下的当前版本信息
                logger.info("Delete manifestRepositoryPath：{}", manifestRepositoryPath.toAbsolutePath());
                this.delete(currentManifestRepositoryPath, force);
            }
            RepositoryPath configRepositoryPath = null;
            //当前版本下的配置信息
            if (Objects.nonNull(itemImageManifest.getConfig())) {
                String currentConfigDigest = itemImageManifest.getConfig().getDigest();
                if (!manifestConfigList.contains(currentConfigDigest)) {
                    //删除blobs目录下的配置信息
                    configRepositoryPath = repositoryPathResolver.resolve(repositoryPath.getStorageId(), repositoryPath.getRepositoryId(), parent.getFileName() + File.separator + blobs + File.separator + currentConfigDigest);
                    logger.info("Delete configRepositoryPath：{}", configRepositoryPath.toAbsolutePath());
                    this.delete(configRepositoryPath, force);
                }
            }
            //删除blobs目录下的层级文件
            if (CollectionUtils.isNotEmpty(currentLayerManifestList)) {
                for (LayerManifest item : currentLayerManifestList) {
                    String blobRepositoryPath = blobs + File.separator + item.getDigest();
                    RepositoryPath itemRepositoryPath = repositoryPathResolver.resolve(repositoryPath.getStorageId(), repositoryPath.getRepositoryId(), parent.getFileName() + File.separator + blobRepositoryPath);
                    logger.info("Delete blobRepositoryPath：{}", itemRepositoryPath.toAbsolutePath());
                    this.delete(itemRepositoryPath, force);
                }
            }
        }
    }

    /**
     * 校验当前版本的manifest文件是否被其他版本使用
     *
     * @param parentRepositoryPath 父级目录
     * @param fileName             文件名称
     * @param directoryName        当前目录名称
     * @return true 在使用 false 不在使用
     */
    public boolean checkRelation(RepositoryPath parentRepositoryPath, String fileName, String directoryName) {
        //镜像顶级路径
        AtomicBoolean flag = new AtomicBoolean(false);
        String blobs = "blobs";
        String manifest = "manifest";
        String sha256 = "sha256";
        List<String> directoryNameList = Lists.newArrayList(blobs, manifest, directoryName);
        try (Stream<Path> pathStream = Files.list(parentRepositoryPath)) {
            pathStream.filter(f -> Files.isDirectory(f) && !directoryNameList.contains(f.getFileName().toString())).forEach(item -> {
                try {
                    boolean b;
                    try (Stream<Path> itemPathStream = Files.list(item)) {
                        b = itemPathStream.filter(f -> !Files.isDirectory(f) && f.getFileName().toString().startsWith(sha256) && !f.getFileName().toString().endsWith(sha256)).anyMatch(fc -> {
                            RepositoryPath itemManifestRepositoryPath = null;
                            List<String> imageManifestNames = Collections.emptyList();
                            try {
                                itemManifestRepositoryPath = parentRepositoryPath.resolve(manifest + File.separator + fc.getFileName().toString());
                                List<ImageManifest> imageManifestList = getImageManifests(itemManifestRepositoryPath);
                                imageManifestNames = Optional.ofNullable(imageManifestList).orElse(Collections.emptyList()).stream().map(ImageManifest::getDigest).collect(Collectors.toList());
                            } catch (IOException ex) {
                                logger.error("获取 {} 的imageManifestList错误 {}", itemManifestRepositoryPath, ExceptionUtils.getStackTrace(ex));
                            }
                            return !Files.isDirectory(fc) && imageManifestNames.contains(fileName);
                        });
                    }
                    if (b) {
                        logger.info("存在关联 {}，版本目录名称：{}", fileName, item.getFileName().toString());
                        flag.set(true);
                    }
                } catch (IOException ex) {
                    logger.error("IO异常：{}", ExceptionUtils.getStackTrace(ex));
                }
            });
        } catch (IOException ex) {
            logger.error("IO异常：{}", ExceptionUtils.getStackTrace(ex));
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

    public List<ImageManifest> getImageManifests(RepositoryPath repositoryPath) throws IOException {
        if (Objects.isNull(repositoryPath) || !Files.exists(repositoryPath)) {
            return null;
        }
        DockerArtifactCoordinates dockerArtifactCoordinates = DockerArtifactCoordinates.parse(RepositoryFiles.relativizePath(repositoryPath));
        String imageName = dockerArtifactCoordinates.getName();
        List<ImageManifest> imageManifestList = Lists.newArrayList();
        String manifestString = Files.readString(repositoryPath);
        ImageManifest imageManifest = JSON.parseObject(manifestString, ImageManifest.class);
        if (CollectionUtils.isNotEmpty(imageManifest.getManifests())) {
            //多架构镜像
            ImageManifest itemImageManifest = null;
            for (Manifests manifests : imageManifest.getManifests()) {
                RepositoryPath manifestPath = repositoryPathResolver.resolve(repositoryPath.getStorageId(), repositoryPath.getRepositoryId(), imageName + "/manifest/" + manifests.getDigest());
                if (Files.exists(manifestPath)) {
                    manifestString = Files.readString(manifestPath);
                    itemImageManifest = JSON.parseObject(manifestString, ImageManifest.class);
                    itemImageManifest.setDigest(manifests.getDigest());
                    imageManifestList.add(itemImageManifest);
                }
            }
        }
        imageManifest.setDigest(dockerArtifactCoordinates.getLayers());
        imageManifestList.add(imageManifest);
        return imageManifestList;
    }

}
