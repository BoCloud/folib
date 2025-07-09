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
package com.folib.controllers.layout.docker;

import com.folib.artifact.coordinates.DockerCoordinates;
import com.folib.controllers.BaseArtifactController;
import com.folib.providers.io.RepositoryPath;
import com.folib.storage.repository.Repository;
import com.folib.utils.DockerUtils;
import com.fasterxml.jackson.core.JsonGenerator;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.folib.components.layout.DockerComponent;
import com.folib.web.LayoutReqMapping;
import com.folib.web.RepoMapping;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.apache.commons.compress.archivers.tar.TarArchiveEntry;
import org.apache.commons.compress.archivers.tar.TarArchiveOutputStream;
import org.apache.commons.lang3.StringUtils;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Lazy;

import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;

import javax.inject.Inject;
import java.io.*;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

@LayoutReqMapping(DockerCoordinates.LAYOUT_NAME)
@RestController
@Api(description = "docker存储空间控制器", tags = "docker存储空间控制器")
public class DockerStorageController extends BaseArtifactController {

    @Inject
    @Lazy
    private DockerComponent dockerComponent;

    @Value("${folib.temp}")
    private String tempPath;
    
    @ApiOperation(value = "Used to deploy an artifact")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "The artifact was deployed successfully."),
            @ApiResponse(code = 400, message = "An error occurred.")})
    @PreAuthorize("hasAuthority('ARTIFACTS_DEPLOY')")
    @PutMapping(value = "{storageId}/{repositoryId}/{artifactPath:.+}")
    public ResponseEntity upload(@RepoMapping Repository repository,
                                 @PathVariable String artifactPath,
                                 HttpServletRequest request) {
        final String storageId = repository.getStorage().getId();
        final String repositoryId = repository.getId();
        try (InputStream is =  request.getInputStream()){
            RepositoryPath repositoryPath = repositoryPathResolver.resolve(storageId, repositoryId, artifactPath);
            if (!DockerUtils.isSubsidiaryFile(repositoryPath)) {
                String msg = String.format("The parent path of the artifact path [%s] must be [%s]", artifactPath, DockerUtils.SUBSIDIARY_PATH);
                return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(msg);
            }
            artifactManagementService.validateAndStore(repositoryPath, is);
            return ResponseEntity.ok("The artifact was deployed successfully.");
        } catch (Exception e) {
            logger.error(e.getMessage(), e);
            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).body(e.getMessage());
        }
    }

    @ApiOperation(value = "Used to retrieve an artifact")
    @ApiResponses(value = {@ApiResponse(code = 200, message = ""),
            @ApiResponse(code = 404, message = "Requested path not found."),
            @ApiResponse(code = 500, message = "Server error."),
            @ApiResponse(code = 503, message = "Repository currently not in service.")})
    @PreAuthorize("hasAuthority('ARTIFACTS_RESOLVE')")
    @RequestMapping(value = {"/{storageId}/{repositoryId}/{artifactPath:.+}"}, method = {RequestMethod.GET, RequestMethod.HEAD})
    public void download(@RepoMapping Repository repository,
                         @PathVariable String artifactPath,
                         @RequestHeader HttpHeaders httpHeaders, HttpServletRequest request,
                         HttpServletResponse response)
            throws Exception {
        final String storageId = repository.getStorage().getId();
        final String repositoryId = repository.getId();
        logger.info("Requested get docker application file {}/{}/{}.", storageId, repositoryId, artifactPath);
        RepositoryPath repositoryPath = repositoryPathResolver.resolve(storageId, repositoryId, artifactPath);
        vulnerabilityBlock(repositoryPath);
        response.setCharacterEncoding("UTF-8");
        response.setHeader("Content-Disposition", "attachment;" + repositoryPath.getFileName().toString());
        provideArtifactDownloadResponse(request, response, httpHeaders, repositoryPath);
    }


    @ApiOperation(value = "Used to retrieve an artifact")
    @ApiResponses(value = {@ApiResponse(code = 200, message = ""),
            @ApiResponse(code = 404, message = "Requested path not found."),
            @ApiResponse(code = 500, message = "Server error."),
            @ApiResponse(code = 503, message = "Repository currently not in service.")})
    @PreAuthorize("hasAuthority('ARTIFACTS_RESOLVE')")
    @RequestMapping(value = {"/{storageId}/{repositoryId}/download/{artifactPath:.+}"}, method = {RequestMethod.GET, RequestMethod.HEAD})
    public void downloadImage(@RepoMapping Repository repository,
                              @PathVariable String artifactPath,
                              @RequestParam(value = "platform", required = false) String platform,
                              @RequestHeader HttpHeaders httpHeaders,
                              HttpServletRequest request,
                              HttpServletResponse response)
            throws Exception {
        final String storageId = repository.getStorage().getId();
        final String repositoryId = repository.getId();
        String imagePath;
        String tag;
        String imageName;
        String regex = "((?:[a-zA-Z0-9-_./]+/)?)([a-zA-Z0-9-_.]+):(.+)";
        Pattern pattern = Pattern.compile(regex);

        //校验镜像路径
        Matcher matcher = pattern.matcher(artifactPath);
        if (matcher.matches()) {
            String repositoryPath = matcher.group(1);
            imageName = matcher.group(2);
            tag = matcher.group(3);
            // If repository is empty, set it to "none" or any default value
            if (repositoryPath.isEmpty()) {
                imagePath = imageName;
            } else {
                imagePath = String.join("/", repositoryPath, imageName);
            }

        } else {
            throw new RuntimeException("Docker Mirror path format error");
        }

        //校验镜像架构
        String targetArchitecture = null;
        if (platform != null) {
            String targetArchitectureRegex = "([a-zA-Z0-9/]+)/([a-zA-Z0-9/]+)";
            String regex2 = "([a-zA-Z0-9/]+)/([a-zA-Z0-9/]+)/([a-zA-Z0-9/]+)";
            Pattern targetArchitecturePattern = Pattern.compile(targetArchitectureRegex);
            Matcher matcher2 = targetArchitecturePattern.matcher(platform);
            Pattern targetArchitectureRegex2 = Pattern.compile(regex2);
            Matcher matcher3 = targetArchitectureRegex2.matcher(platform);
            if (matcher2.matches() || matcher3.matches()) {
                targetArchitecture = matcher.matches() ? matcher2.group(2) : matcher3.group(2);
            } else {
                throw new RuntimeException("Docker Mirror path format error");
            }
        }

        //获取镜像的manifest
        JsonNode manifest = handleManifest(storageId, repositoryId, imagePath, tag, targetArchitecture);

        //设置零时目录
        String uuid = UUID.randomUUID().toString();
        Path tempPaths = Paths.get(String.join("/", tempPath, uuid));
        Files.createDirectories(tempPaths);
        String tarName = targetArchitecture == null ? String.join("", imageName, "-",tag, ".tar") :String.join("", imageName, "-",targetArchitecture ,"-",tag, ".tar");

        // 获取镜像的 config
        String configDigest = manifest.get("config").get("digest").asText();
        String config = getConfig(storageId, repositoryId, imagePath, configDigest);

        //下载镜像层
        downloadLayers(storageId, repositoryId, imagePath, manifest, config, configDigest, tag, String.join("/", tempPaths.toString(), tarName));
        //设置文件下载名称和类型
        response.setHeader(HttpHeaders.CONTENT_DISPOSITION, "attachment; filename=\"" + tarName + "\"");
        response.setContentType("application/octet-stream");
        InputStream is = Files.newInputStream(Path.of(String.join("/", tempPaths.toString(), tarName)));
        copyToResponse(is, response);
        is.close();
        //删除零时文件和目录
        Files.walk(tempPaths)
                .sorted(Comparator.reverseOrder())
                .map(Path::toFile)
                .forEach(File::delete);
    }


    private JsonNode  handleManifest(String storageId, String repositoryId, String repository, String tag,String targetArchitecture) throws Exception {

        if(targetArchitecture == null){
           return getManifestByDigest( storageId,  repositoryId,  repository,  tag);
        }

        // 获取多架构 manifest
        JsonNode multiArchManifest = getManifest( storageId,  repositoryId,repository, tag);
        if (!multiArchManifest.has("manifests")) {
            throw new IllegalArgumentException("The manifest does not contain multiple architectures.");
        }
        // 选择特定架构的 manifest
        JsonNode manifest = selectArchitectureManifest(storageId,  repositoryId,repository, multiArchManifest, targetArchitecture);
        // 获取镜像的 config
        if (!manifest.has("config")) {
            throw new IllegalArgumentException("The selected manifest does not contain a config.");
        }
        return manifest;
    }


    private JsonNode getManifest(String storageId, String repositoryId, String repository, String tag) throws IOException {

        RepositoryPath repositoryPath = dockerComponent.resolveManifest(storageId, repositoryId, repository, tag);
        if (Files.exists(repositoryPath)) {
            ObjectMapper mapper = new ObjectMapper();
            return mapper.readTree(Files.readString(repositoryPath));

        } else {
            //docker 镜像不存在
            throw new RuntimeException("the docker image does not exist ");
        }

    }

    private JsonNode selectArchitectureManifest(String storageId, String repositoryId, String repository, JsonNode multiArchManifest, String targetArchitecture) throws Exception {
        if (multiArchManifest.has("manifests")) {
            for (JsonNode manifest : multiArchManifest.get("manifests")) {
                if (manifest.get("platform").get("architecture").asText().equals(targetArchitecture)) {
                    String digest = manifest.get("digest").asText();
                    return getManifestByDigest(storageId, repositoryId, repository, digest);
                }
            }
        }
        throw new IllegalArgumentException("No manifest found for architecture: " + targetArchitecture);
    }

    private JsonNode getManifestByDigest(String storageId, String repositoryId, String repository, String digest) throws Exception {
        RepositoryPath repositoryPath = dockerComponent.resolveManifest(storageId, repositoryId, repository, digest);
        if (Files.exists(repositoryPath)) {
            ObjectMapper mapper = new ObjectMapper();
            return mapper.readTree(Files.readString(repositoryPath));
        } else {
            //docker 镜像不存在
            throw new RuntimeException("the docker image does not exist ");
        }
    }

    private String getConfig(String storageId, String repositoryId, String repository, String configDigest) throws Exception {
        String targetUrl = String.format("%s/blobs/%s", StringUtils.removeEnd(repository, "/"), configDigest);
        String artifactPath = String.format("blobs/%s", configDigest);
        RepositoryPath repositoryPath = repositoryPathResolver.resolve(storageId, repositoryId, artifactPath);
        if (!artifactRealExists(repositoryPath)) {
            repositoryPath.setTargetUrl(targetUrl);
            repositoryPath.setArtifactPath(repository);
            repositoryPath = artifactResolutionService.resolvePath(repositoryPath);

        }
        if (artifactRealExists(repositoryPath)) {
            return Files.lines(Paths.get(repositoryPath.toString()), StandardCharsets.UTF_8).collect(Collectors.joining(System.lineSeparator()));
        }
        throw new RuntimeException("the docker image config does not exist ");
    }

    private RepositoryPath getLayerPath(String storageId, String repositoryId, String repository, String digest) throws IOException {
        String targetUrl = String.format("%s/blobs/%s", StringUtils.removeEnd(repository, "/"), digest);
        String artifactPath = String.format("blobs/%s", digest);
        RepositoryPath repositoryPath = repositoryPathResolver.resolve(storageId, repositoryId, artifactPath);
        if (!artifactRealExists(repositoryPath)) {
            repositoryPath.setTargetUrl(targetUrl);
            repositoryPath.setArtifactPath(repository);
            repositoryPath = artifactResolutionService.resolvePath(repositoryPath);

        }
        if (artifactRealExists(repositoryPath)) {
            return repositoryPath;
        }
        throw new RuntimeException("the docker image config does not exist ");
    }

    private void downloadLayers(String storageId, String repositoryId, String repository, JsonNode manifest, String config, String configDigest, String tag, String outputPath) throws Exception {

        JsonNode layers = manifest.get("layers");
        List<String> layerFiles = new ArrayList<>();

        // 创建 tar 文件输出流
        try (FileOutputStream fos = new FileOutputStream(outputPath);
             TarArchiveOutputStream tarOutput = new TarArchiveOutputStream(fos)) {

            // 下载每个层并写入 tar 文件
            for (JsonNode layer : layers) {
                String layerDigest = layer.get("digest").asText();
                RepositoryPath layerPath = getLayerPath(storageId, repositoryId, repository, layerDigest);
                try (InputStream layerStream = Files.newInputStream(layerPath)) {
                    String layerFileName = layerDigest.replace(":", "_") + ".tar";
                    layerFiles.add(layerFileName);
                    TarArchiveEntry tarEntry = new TarArchiveEntry(layerFileName);
                    tarEntry.setSize(layerPath.getArtifactEntry().getSizeInBytes());
                    tarOutput.putArchiveEntry(tarEntry);

                    byte[] buffer = new byte[8192];
                    int bytesRead;
                    while ((bytesRead = layerStream.read(buffer)) != -1) {
                        tarOutput.write(buffer, 0, bytesRead);
                    }
                    tarOutput.closeArchiveEntry();
                }
            }

            // 写入 config.json
            writeConfigJson(tarOutput, config);

            // 写入 manifest.json
            writeManifestJson(tarOutput, layerFiles, repository, tag);

            // 写入 repositories 文件
            writeRepositoriesFile(tarOutput, repository, tag, configDigest);

            System.out.println("Tar file created: " + outputPath);
        }

    }

    private void writeConfigJson(TarArchiveOutputStream tarOutput, String config) throws IOException {
        TarArchiveEntry configEntry = new TarArchiveEntry("config.json");
        byte[] configBytes = config.getBytes(StandardCharsets.UTF_8);
        configEntry.setSize(configBytes.length);
        tarOutput.putArchiveEntry(configEntry);
        tarOutput.write(configBytes);
        tarOutput.closeArchiveEntry();
    }

    private void writeManifestJson(TarArchiveOutputStream tarOutput, List<String> layerFiles, String repository, String tag) throws IOException {
        List<Map<String, Object>> manifest = new ArrayList<>();
        Map<String, Object> manifestItem = new HashMap<>();
        manifestItem.put("Config", "config.json");
        manifestItem.put("RepoTags", new String[]{repository + ":" + tag});
        manifestItem.put("Layers", layerFiles);
        manifest.add(manifestItem);

        TarArchiveEntry manifestEntry = new TarArchiveEntry("manifest.json");
        StringWriter stringWriter = new StringWriter();
        ObjectMapper mapper = new ObjectMapper();
        JsonGenerator jsonGenerator = mapper.getFactory().createGenerator(stringWriter);
        jsonGenerator.writeTree(mapper.valueToTree(manifest));
        jsonGenerator.close();

        String manifestJson = stringWriter.toString();
        byte[] manifestBytes = manifestJson.getBytes(StandardCharsets.UTF_8);
        manifestEntry.setSize(manifestBytes.length);
        tarOutput.putArchiveEntry(manifestEntry);
        tarOutput.write(manifestBytes);
        tarOutput.closeArchiveEntry();
    }

    private void writeRepositoriesFile(TarArchiveOutputStream tarOutput, String repository, String tag, String configDigest) throws IOException {
        Map<String, Map<String, String>> repositoriesMap = new HashMap<>();
        Map<String, String> repoTagMap = new HashMap<>();
        repoTagMap.put(tag, configDigest);
        repositoriesMap.put(repository, repoTagMap);

        TarArchiveEntry repoEntry = new TarArchiveEntry("repositories");
        StringWriter stringWriter = new StringWriter();
        ObjectMapper mapper = new ObjectMapper();
        JsonGenerator jsonGenerator = mapper.getFactory().createGenerator(stringWriter);
        jsonGenerator.writeTree(mapper.valueToTree(repositoriesMap));
        jsonGenerator.close();

        String repoJson = stringWriter.toString();
        byte[] repoBytes = repoJson.getBytes(StandardCharsets.UTF_8);
        repoEntry.setSize(repoBytes.length);
        tarOutput.putArchiveEntry(repoEntry);
        tarOutput.write(repoBytes);
        tarOutput.closeArchiveEntry();
    }
}
