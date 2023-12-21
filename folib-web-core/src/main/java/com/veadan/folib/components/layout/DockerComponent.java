package com.veadan.folib.components.layout;

import com.alibaba.fastjson.JSON;
import com.google.common.collect.Lists;
import com.veadan.folib.artifact.coordinates.DockerArtifactCoordinates;
import com.veadan.folib.domain.DirectoryListing;
import com.veadan.folib.domain.FileContent;
import com.veadan.folib.providers.io.RepositoryFiles;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.providers.io.RepositoryPathResolver;
import com.veadan.folib.schema2.ImageManifest;
import com.veadan.folib.schema2.LayerManifest;
import com.veadan.folib.schema2.Manifests;
import com.veadan.folib.services.DirectoryListingService;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Component;

import javax.inject.Inject;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/**
 * @author leipenghui
 **/
@Slf4j
@Component
public class DockerComponent {

    private static final String PREFIX = "sha256:";

    @Inject
    private RepositoryPathResolver repositoryPathResolver;

    @Inject
    @Qualifier("browseRepositoryDirectoryListingService")
    private volatile DirectoryListingService directoryListingService;

    public List<ImageManifest> getImageManifests(RepositoryPath repositoryPath) throws Exception {
        log.info("Get manifest param [{}]", repositoryPath);
        if (!Files.exists(repositoryPath)) {
            return null;
        }
        if (Files.isDirectory(repositoryPath)) {
            DirectoryListing directoryListing = directoryListingService.fromRepositoryPath(repositoryPath);
            List<FileContent> fileContents = directoryListing.getFiles().stream().filter(file -> DockerArtifactCoordinates.isManifestPath(file.getName())).collect(Collectors.toList());
            if (CollectionUtils.isEmpty(fileContents)) {
                return null;
            }
            FileContent fileContent = fileContents.get(0);
            repositoryPath = repositoryPathResolver.resolve(repositoryPath.getStorageId(), repositoryPath.getRepositoryId(), fileContent.getArtifactPath());
        }
        if (!Files.exists(repositoryPath)) {
            return null;
        }
        DockerArtifactCoordinates dockerArtifactCoordinates = DockerArtifactCoordinates.parse(RepositoryFiles.relativizePath(repositoryPath));
        String imageName = dockerArtifactCoordinates.getName();
        List<ImageManifest> imageManifestList = Lists.newArrayList();
        String manifestString = readManifest(repositoryPath);
        ImageManifest imageManifest = JSON.parseObject(manifestString, ImageManifest.class);
        if (CollectionUtils.isNotEmpty(imageManifest.getManifests())) {
            //多架构镜像
            ImageManifest itemImageManifest = null;
            for (Manifests manifests : imageManifest.getManifests()) {
                RepositoryPath manifestPath = repositoryPathResolver.resolve(repositoryPath.getStorageId(), repositoryPath.getRepositoryId(),  "manifest/" + manifests.getDigest());
                manifestString = readManifest(manifestPath);
                itemImageManifest = JSON.parseObject(manifestString, ImageManifest.class);
                itemImageManifest.setDigest(manifests.getDigest());
                imageManifestList.add(itemImageManifest);
            }
        }
        imageManifest.setDigest(dockerArtifactCoordinates.getLayers());
        imageManifestList.add(imageManifest);
        return imageManifestList;
    }

    public List<LayerManifest> getImageLayers(RepositoryPath repositoryPath) {
        try {
            DockerArtifactCoordinates dockerArtifactCoordinates = DockerArtifactCoordinates.parse(RepositoryFiles.relativizePath(repositoryPath));
            String imageName = dockerArtifactCoordinates.getName();
            List<LayerManifest> layerManifests = Lists.newArrayList();
            String manifestString = readManifest(repositoryPath);
            ImageManifest imageManifest = JSON.parseObject(manifestString, ImageManifest.class);
            if (CollectionUtils.isNotEmpty(imageManifest.getLayers())) {
                layerManifests.addAll(imageManifest.getLayers());
            }
            if (CollectionUtils.isNotEmpty(imageManifest.getManifests())) {
                //多架构镜像
                ImageManifest itemImageManifest = null;
                for (Manifests manifests : imageManifest.getManifests()) {
                    RepositoryPath manifestPath = repositoryPathResolver.resolve(repositoryPath.getStorageId(), repositoryPath.getRepositoryId(), "manifest/" + manifests.getDigest());
                    manifestString = readManifest(manifestPath);
                    itemImageManifest = JSON.parseObject(manifestString, ImageManifest.class);
                    if (CollectionUtils.isNotEmpty(itemImageManifest.getLayers())) {
                        layerManifests.addAll(itemImageManifest.getLayers());
                    }
                }
            }
            return layerManifests;
        } catch (Exception ex) {
            log.error("获取镜像层级列表错误：{}", ExceptionUtils.getStackTrace(ex));
            return null;
        }
    }

    private String readManifest(RepositoryPath manifestPath) {
        if (!DockerArtifactCoordinates.isManifestPath(manifestPath)) {
            throw new IllegalArgumentException(String.format("RepositoryPath [%s] not is a manifest path", manifestPath));
        }
        try {
            return Files.readString(manifestPath);
        } catch (Exception ex) {
            log.warn("Read manifestPath [{}] error [{}]", manifestPath, ExceptionUtils.getStackTrace(ex));
            throw new RuntimeException(ex);
        }
    }

    /**
     * 获取仓库下的tag路径
     *
     * @param repositoryPath 仓库路径
     * @return tag路径
     * @throws IOException 异常
     */
    public RepositoryPath getManifestPath(RepositoryPath repositoryPath)
            throws IOException {
        RepositoryPath path = null;
        List<String> excludeList = Lists.newArrayList("temp", ".temp");
        try (Stream<Path> pathStream = Files.list(repositoryPath)) {
            List<Path> pathList = pathStream.filter(p -> {
                try {
                    RepositoryPath itemRepositoryPath = (RepositoryPath) p;
                    return p.getFileName().toString().startsWith(PREFIX) && !p.toString().startsWith(".") && !p.toString().contains("/.") && excludeList.stream().noneMatch(p.getFileName().toString()::equals) &&
                            !Files.isHidden(p) && !Files.isDirectory(p) && !RepositoryFiles.isChecksum(itemRepositoryPath) && !RepositoryFiles.isArtifactMetadata(itemRepositoryPath);
                } catch (IOException e) {
                    log.warn("Error accessing path [{}] error [{}]", p, ExceptionUtils.getStackTrace(e));
                    return false;
                }
            })
                    .sorted()
                    .collect(Collectors.toList());
            if (CollectionUtils.isNotEmpty(pathList)) {
                path = (RepositoryPath) pathList.get(0);
                path.setArtifact(repositoryPathResolver.resolve(path.getStorageId(), path.getRepositoryId(), RepositoryFiles.relativizePath(path)).getArtifactEntry());

            }
            log.info("Tag [{}] manifestRepositoryPath [{}]", repositoryPath, path);
        }
        return path;
    }
}
