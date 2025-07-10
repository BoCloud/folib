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
package com.folib.controllers.adapter.jfrog;

import cn.hutool.core.date.DateUtil;
import cn.hutool.json.JSONUtil;
import com.folib.constant.GlobalConstants;
import com.folib.dto.ArtifactUploadAdapterJfrogDto;
import com.folib.dto.ArtifactUploadAdapterJfrogDto.Checksums;
import com.folib.dto.ArtifactUploadAdapterJfrogDto.OriginalChecksums;
import com.folib.promotion.ArtifactUploadTask;
import com.folib.promotion.PromotionUtil;
import com.folib.providers.io.RepositoryFiles;
import com.folib.providers.io.RepositoryPath;
import com.folib.providers.layout.LayoutProviderRegistry;
import com.folib.repositories.ArtifactRepository;
import com.folib.repository.MavenRepositoryFeatures;
import com.folib.services.ArtifactManagementService;
import com.folib.services.ArtifactMetadataService;
import com.folib.services.RepositoryManagementService;
import com.folib.utils.UserUtils;
import io.swagger.annotations.Api;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Lazy;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;

import javax.activation.MimetypesFileTypeMap;
import javax.inject.Inject;
import jakarta.servlet.http.HttpServletRequest;
import java.io.InputStream;
import java.nio.file.Files;
import java.util.*;

/**
 * @author veadan
 */
@Slf4j
@RequestMapping("/artifactory")
@RestController
@PreAuthorize("hasAuthority('ADMIN')")
@Api(description = "JFrog上传", tags = "JFrog上传")
public class ArtifactUploadController extends JFrogBaseController {
    @Inject
    private RepositoryManagementService repositoryManagementService;
    @Inject
    private ArtifactManagementService artifactManagementService;
    @Inject
    private ArtifactMetadataService artifactMetadataService;

    @Autowired
    private PromotionUtil promotionUtil;
    @Inject
    private LayoutProviderRegistry layoutProviderRegistry;
    @Inject
    private ArtifactRepository artifactRepository;
    @Inject
    @Lazy
    private MavenRepositoryFeatures mavenRepositoryFeatures;

    @Value("${folib.temp}")
    private String tempPath;

    @PreAuthorize("authenticated")
    @PutMapping(value = "/{repositoryId}/{artifactPath:.+}")
    public ResponseEntity<?> upload(@PathVariable String repositoryId,
                                    @PathVariable String artifactPath,
                                    @RequestParam(value = "uuid", required = false) String uuid,
                                    @RequestParam(value = "metaData", required = false) String metaData,
                                    HttpServletRequest request) throws Exception {
        String storageId = getDefaultStorageId(repositoryId);
        boolean checkRepository = checkRepository(storageId, repositoryId);
        if (!checkRepository) {
            return repositoryNotFound();
        }
        final InputStream inputStream = request.getInputStream();
        final String baseUrl = StringUtils.chomp(configurationManager.getConfiguration().getBaseUrl(), "/");
        final String userName = UserUtils.getUsername();

        final ArtifactUploadTask artifactUploadTask = new ArtifactUploadTask(storageId, repositoryId, inputStream,
                repositoryPathResolver, artifactManagementService, promotionUtil,
                layoutProviderRegistry, artifactMetadataService, artifactRepository, mavenRepositoryFeatures,
                tempPath, artifactPath, metaData, uuid, null);
        final String msg = artifactUploadTask.call();
        if (StringUtils.isNotBlank(msg)) {
            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).body(msg);
        }

        RepositoryPath repositoryPath;
        if (Objects.nonNull(artifactUploadTask.getRepositoryPath())) {
            repositoryPath = artifactUploadTask.getRepositoryPath();
        } else {
            repositoryPath = repositoryPathResolver.resolve(storageId, repositoryId, artifactPath);
        }
        if (Objects.isNull(repositoryPath.getArtifactEntry())) {
            return ResponseEntity.status(HttpStatus.NOT_FOUND).body(String.format(GlobalConstants.ARTIFACT_NOT_FOUND_MESSAGE, storageId, repositoryId, artifactPath));
        }
        final String finalArtifactPath = RepositoryFiles.relativizePath(repositoryPath);
        final String fileDownUrl = String.format("%s/artifactory/%s/%s", baseUrl, repositoryId, finalArtifactPath);
        final Map<String, String> checksums = Optional.ofNullable(repositoryPath.getArtifactEntry().getChecksums()).orElse(Collections.emptyMap());
        final String sha256 = checksums.get("SHA-256");
        final ArtifactUploadAdapterJfrogDto artifactUploadAdapterJfrogDto = new ArtifactUploadAdapterJfrogDto();
        artifactUploadAdapterJfrogDto.setRepo(repositoryId);
        artifactUploadAdapterJfrogDto.setPath(finalArtifactPath);
        artifactUploadAdapterJfrogDto.setCreated(DateUtil.format(new Date(), "yyyy-MM-dd HH:mm:ss"));
        artifactUploadAdapterJfrogDto.setCreatedBy(userName);
        artifactUploadAdapterJfrogDto.setDownloadUri(fileDownUrl);
        MimetypesFileTypeMap mimetypesFileTypeMap = new MimetypesFileTypeMap();
        String mimeType = mimetypesFileTypeMap.getContentType(RepositoryFiles.relativizePath(repositoryPath));
        artifactUploadAdapterJfrogDto.setMimeType(mimeType);
        artifactUploadAdapterJfrogDto.setSize(Files.size(repositoryPath) + "");
        artifactUploadAdapterJfrogDto.setChecksums(new Checksums()
                .setMd5(checksums.get("MD5"))
                .setSha1(checksums.get("SHA-1"))
                .setSha256(sha256));
        artifactUploadAdapterJfrogDto.setOriginalChecksums(new OriginalChecksums().setSha256(sha256));
        artifactUploadAdapterJfrogDto.setUri(fileDownUrl);

        return ResponseEntity.ok(JSONUtil.toJsonStr(JSONUtil.parse(artifactUploadAdapterJfrogDto), 2));
    }
}
