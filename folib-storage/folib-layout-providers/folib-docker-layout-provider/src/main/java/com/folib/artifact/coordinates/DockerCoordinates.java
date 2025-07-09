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
package com.folib.artifact.coordinates;

import com.google.common.collect.Lists;
import com.folib.db.schema.Vertices;
import com.folib.domain.LayoutCoordinatesEntity;
import com.folib.providers.io.RepositoryFiles;
import com.folib.providers.io.RepositoryPath;
import com.folib.utils.DockerUtils;
import jakarta.xml.bind.annotation.XmlAccessType;
import jakarta.xml.bind.annotation.XmlAccessorType;
import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlRootElement;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.neo4j.ogm.annotation.NodeEntity;

import java.net.URI;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/**
 * @author Veadan
 */
@Slf4j
@NodeEntity(Vertices.DOCKER_COORDINATES)
@XmlRootElement(name = "DockerCoordinates")
@XmlAccessorType(XmlAccessType.NONE)
@CoordinatesLayout(name = DockerCoordinates.LAYOUT_NAME, alias = DockerCoordinates.LAYOUT_ALIAS)
public class DockerCoordinates
        extends LayoutCoordinatesEntity<DockerCoordinates, String> {

    public static final String LAYOUT_NAME = "Docker";

    public static final String LAYOUT_ALIAS = "Docker";

    //public static final String REPOSITORY = "repository";
    public static final String IMAGE_NAME = "name";

    public static final String TAG = "tag";

    public static final String LAYERS = "layers";

    public static final String ARTIFACT_PATH = "path";

    public static final String SHA_256 = "sha256";

    public static final String CHECKSUM_SHA_256 = ".sha256";

    public static final String SELF_METADATA = ".metadata";

    public static final String FO_LIBRARY_METADATA = ".foLibrary-metadata";

    public static final List<String> DOCKER_LAYER_DIR_NAME_LIST = Lists.newArrayList("blobs", "manifest");

    public DockerCoordinates() {
        resetCoordinates(LAYERS, ARTIFACT_PATH);
    }


    //
    // TODO: We will have to think about something like this:
    //


    public DockerCoordinates(String repository,
                             String reference) {
        // if any of the required arguments are empty, throw an error
        if (StringUtils.isBlank(repository)) {
            throw new IllegalArgumentException("The repository field is mandatory.");
        }

        if (StringUtils.isBlank(reference)) {
            throw new IllegalArgumentException("The reference field is mandatory.");
        }

        setId(repository);
        setVersion(reference);

        // TODO:
        // setLayers(layers);
    }

    public DockerCoordinates(String imageName,
                             String reference,
                             String layers,
                             String artifactPath) {
        // if any of the required arguments are empty, throw an error
        if (StringUtils.isBlank(imageName)) {
            throw new IllegalArgumentException("The imageName field is mandatory.");
        }

//        if (StringUtils.isBlank(reference))
//        {
//            throw new IllegalArgumentException("The reference field is mandatory.");
//        }

        setId(imageName);
        setVersion(reference);
        setTAG(reference);
        setLayers(layers);
        setArtifactPath(artifactPath);

        // TODO:
        // setLayers(layers);
    }

    // todo 优化
    public static DockerCoordinates parse(String path) {
        // TODO:
        if (Objects.isNull(path)) {
            return null;
        }
        String tag = null;
        String[] strings = null;
        strings = path.split("/");
        if (!path.contains("/blobs/") && strings.length >= 2) {
            tag = strings[strings.length - 2];
        } else {
            tag = "v2";
        }
        String layers = strings[strings.length - 1];
        String finalTag = tag;
        String imageName = Arrays.stream(strings).filter(data -> !Objects.equals(layers, data) && !Objects.equals(finalTag, data))
                .collect(Collectors.joining("/"));
        if (StringUtils.isBlank(imageName)) {
            imageName = finalTag;
        }
        String artifactPath = "";
        String finalImageName = tag;
        if (strings[strings.length - 1].contains("sha256:")) {
            artifactPath = Arrays.stream(strings).filter(data -> !Objects.equals(layers, data) || !Objects.equals(finalImageName, data))
                    .collect(Collectors.joining("/"));

        } else if (strings[strings.length - 1].contains("manifest.json")) {
            artifactPath = Arrays.stream(strings).filter(data -> !Objects.equals(layers, data) || !Objects.equals(finalImageName, data))
                    .collect(Collectors.joining("/"));
        } else if (path.contains(DockerUtils.SUBSIDIARY)) {
            artifactPath = path;
        }
        if (StringUtils.isBlank(artifactPath)) {
            throw new IllegalArgumentException(String.format("Path [%s] not a standard Docker layout file", path));
        }
        log.debug("Docker path [{}] tag [{}] imageName [{}] artifactPath [{}] layers [{}]", path, tag, imageName, artifactPath, layers);
        return new DockerCoordinates(imageName, tag, layers, artifactPath);
    }

    public String getIMAGE_NAME() {
        String str = getArtifactPath().replace("/" + getLayers(), "");
        str = StringUtils.reverse(str);
        str = str.replaceFirst("/", ":");
        return StringUtils.reverse(str);
    }

    public void setIMAGE_NAME(String imageName) {
        setCoordinate(IMAGE_NAME, imageName);
    }

    @Override
    @XmlAttribute(name = "imageName")
    public String getId() {
        return getIMAGE_NAME();
    }


    public void setId(String id) {
        setIMAGE_NAME(id);
    }

    @Override
    @XmlAttribute(name = "version")
    public String getVersion() {
        return super.getVersion();
    }

    @Override
    @XmlAttribute(name = "path")
    public String getPath() {
        return super.getPath();
    }

    @ArtifactLayoutCoordinate
    @XmlAttribute(name = "name")
    public String getName() {
        return getCoordinate(IMAGE_NAME);
    }

    @Override
    public void setVersion(String version) {
        //setCoordinate(TAG,version);
        super.setVersion(version);
    }


    /**
     * @return Returns the reconstructed path from the stored coordinate values
     */

    public String toPath() {
        // TODO:
        return ARTIFACT_PATH;
    }

    /**
     * @return Returns the native version of the package
     */
    @Override
    public String getNativeVersion() {
        return getVersion();
    }

    /**
     * @return Returns a map data structure of the coordinates without the TAG coordinate
     */

    public Map<String, String> dropVersion() {
        Map<String, String> result = getCoordinates();
        result.remove(super.getVersion());

        return result;
    }

    @Override
    public GenericCoordinates getHierarchyChild() {
        return super.getHierarchyChild();
    }

    @Override
    public void setHierarchyChild(GenericCoordinates node) {
        super.setHierarchyChild(node);
    }

    @Override
    public String convertToPath(DockerCoordinates artifactCoordinates) {
        return artifactCoordinates.getArtifactPath();
        // return new  DockerArtifactGenerator(artifactCoordinates.getPath()).getImageManifestPath().toString();
        //  return String.format("%s/%s/%s/%s", artifactCoordinates.g, c.getName(), c.getVersion(), c.getArtifactFileName());
    }

    @Override
    public URI convertToResource(DockerCoordinates artifactCoordinates) {
        return super.convertToResource(artifactCoordinates);
    }


    @Override
    public void setUuid(String uuid) {
        super.setUuid(uuid);
    }


    @ArtifactLayoutCoordinate
    @XmlAttribute(name = "tag")
    public String getTAG() {
        return getCoordinate(TAG);
    }

    public void setTAG(String tag) {
        setCoordinate(TAG, tag);
    }

    @ArtifactLayoutCoordinate
    public String getLayers() {
        return getCoordinate(LAYERS);
    }

    public void setLayers(String layers) {
        setCoordinate(LAYERS, layers);
    }


    @ArtifactLayoutCoordinate
    @XmlAttribute(name = "artifactPath")
    public String getArtifactPath() {
        return getCoordinate(ARTIFACT_PATH);
    }

    public void setArtifactPath(String artifactPath) {
        setCoordinate(ARTIFACT_PATH, artifactPath);
    }

    public static boolean isManifestPath(Path path) {
        try {
            if (Objects.isNull(path) || Files.notExists(path) || Files.isDirectory(path) || RepositoryFiles.isHidden(path)) {
                return false;
            }
            String name = path.getFileName().toString();
            return name.startsWith(SHA_256) && !name.endsWith(CHECKSUM_SHA_256) && !name.endsWith(SELF_METADATA) && !name.endsWith(FO_LIBRARY_METADATA) && !path.toString().contains("blobs/sha256");
        } catch (Exception ex) {
            log.warn(ExceptionUtils.getStackTrace(ex));
            return false;
        }
    }

    public static boolean isManifestPath(String name) {
        if (StringUtils.isBlank(name)) {
            return false;
        }
        return name.startsWith(SHA_256) && !name.endsWith(CHECKSUM_SHA_256) && !name.endsWith(SELF_METADATA) && !name.endsWith(FO_LIBRARY_METADATA) && !name.contains("blobs");
    }

    public static boolean isRealManifestPath(Path path) {
        try {
            if (Objects.isNull(path) || Files.notExists(path) || Files.isDirectory(path) || RepositoryFiles.isHidden(path)) {
                return false;
            }
            String name = path.getFileName().toString();
            return name.startsWith(SHA_256) && !name.endsWith(CHECKSUM_SHA_256) && !name.endsWith(SELF_METADATA) && !name.endsWith(FO_LIBRARY_METADATA) && path.toString().contains("manifest/sha256");
        } catch (Exception ex) {
            log.warn(ExceptionUtils.getStackTrace(ex));
            return false;
        }
    }

    public static boolean isDockerTag(RepositoryPath path) {
        try {
            if (Objects.isNull(path) || Files.notExists(path) || RepositoryFiles.isHidden(path)) {
                return false;
            }
            String fullPath = path.toString();
            String relativizePath = RepositoryFiles.relativizePath(path);
            int deepSize = relativizePath.split("/").length;
            int two = 2;
            if (deepSize < two) {
                return false;
            }
            if (Files.isDirectory(path)) {
                try (Stream<Path> pathStream = Files.list(path)) {
                    return pathStream.anyMatch(DockerCoordinates::isTagPath);
                }
            }
            String name = path.getFileName().toString();
            return name.startsWith(SHA_256) && !name.endsWith(CHECKSUM_SHA_256) && !name.endsWith(SELF_METADATA) && !name.endsWith(FO_LIBRARY_METADATA) && !fullPath.contains("blobs/sha256") && !fullPath.contains("manifest/sha256");
        } catch (Exception ex) {
            log.warn(ExceptionUtils.getStackTrace(ex));
            return false;
        }
    }

    public static boolean isTagPath(Path path) {
        try {
            if (Objects.isNull(path) || Files.notExists(path) || Files.isDirectory(path) || RepositoryFiles.isHidden(path)) {
                return false;
            }
            String fullPath = path.toString();
            String name = path.getFileName().toString();
            return name.startsWith(SHA_256) && !name.endsWith(CHECKSUM_SHA_256) && !name.endsWith(SELF_METADATA) && !name.endsWith(FO_LIBRARY_METADATA) && !fullPath.contains("blobs/sha256") && !fullPath.contains("manifest/sha256");
        } catch (Exception ex) {
            log.warn(ExceptionUtils.getStackTrace(ex));
            return false;
        }
    }

    public static boolean exclude(String name) {
        if (StringUtils.isBlank(name)) {
            return true;
        }
        return name.endsWith(CHECKSUM_SHA_256) || name.endsWith(SELF_METADATA) || !name.endsWith(FO_LIBRARY_METADATA);
    }

    public static boolean include(String name) {
        if (StringUtils.isBlank(name)) {
            return true;
        }
        return name.contains(SHA_256) && !name.endsWith(CHECKSUM_SHA_256) && !name.endsWith(SELF_METADATA) && !name.endsWith(FO_LIBRARY_METADATA);
    }

}
