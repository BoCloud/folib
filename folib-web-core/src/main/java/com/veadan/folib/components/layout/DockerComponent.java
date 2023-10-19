package com.veadan.folib.components.layout;

import com.alibaba.fastjson.JSON;
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
import org.apache.commons.compress.utils.Lists;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Component;

import javax.inject.Inject;
import java.nio.file.Files;
import java.util.List;
import java.util.stream.Collectors;

/**
 * @author leipenghui
 **/
@Slf4j
@Component
public class DockerComponent {

    @Inject
    private RepositoryPathResolver repositoryPathResolver;

    @Inject
    @Qualifier("browseRepositoryDirectoryListingService")
    private volatile DirectoryListingService directoryListingService;

    public List<ImageManifest> getImageManifests(RepositoryPath repositoryPath) throws Exception {
        if (Files.isDirectory(repositoryPath)) {
            DirectoryListing directoryListing = directoryListingService.fromRepositoryPath(repositoryPath);
            List<FileContent> fileContents = directoryListing.getFiles().stream().filter(file -> !(file.getName().endsWith(".sha256"))).collect(Collectors.toList());
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
        String manifestString = Files.readString(repositoryPath);
        ImageManifest imageManifest = JSON.parseObject(manifestString, ImageManifest.class);
        if (CollectionUtils.isNotEmpty(imageManifest.getManifests())) {
            //多架构镜像
            ImageManifest itemImageManifest = null;
            for (Manifests manifests : imageManifest.getManifests()) {
                RepositoryPath manifestPath = repositoryPathResolver.resolve(repositoryPath.getStorageId(), repositoryPath.getRepositoryId(), imageName + "/manifest/" + manifests.getDigest());
                manifestString = Files.readString(manifestPath);
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
            String manifestString = Files.readString(repositoryPath);
            ImageManifest imageManifest = JSON.parseObject(manifestString, ImageManifest.class);
            if (CollectionUtils.isNotEmpty(imageManifest.getLayers())) {
                layerManifests.addAll(imageManifest.getLayers());
            }
            if (CollectionUtils.isNotEmpty(imageManifest.getManifests())) {
                //多架构镜像
                ImageManifest itemImageManifest = null;
                for (Manifests manifests : imageManifest.getManifests()) {
                    RepositoryPath manifestPath = repositoryPathResolver.resolve(repositoryPath.getStorageId(), repositoryPath.getRepositoryId(), imageName + "/manifest/" + manifests.getDigest());
                    manifestString = Files.readString(manifestPath);
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
}
