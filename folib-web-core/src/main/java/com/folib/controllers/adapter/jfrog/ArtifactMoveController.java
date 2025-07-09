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

import com.alibaba.fastjson.JSONObject;
import com.google.common.collect.Maps;
import com.folib.domain.ArtifactPromotion;
import com.folib.dto.TargetRepositoyDto;
import com.folib.services.ArtifactPromotionService;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.compress.utils.Lists;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;
import java.util.Map;

@Slf4j
@RequestMapping("/artifactory")
@RestController
@Api(description = "JFrog移动", tags = "JFrog移动")
public class ArtifactMoveController extends JFrogBaseController {

    @Autowired
    private ArtifactPromotionService artifactPromotionService;


    @ApiOperation(value = "JFrog移动")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "OK")})
    @PostMapping("/api/move/{repositoryId}/{artifactPath}")
    public ResponseEntity<?> move(@PathVariable("repositoryId") String repositoryId,
                                  @PathVariable("artifactPath") String artifactPath,
                                  @RequestParam("to") String targetPath,
                                  @RequestParam(value = "dry", defaultValue = "1") int dry,
                                  @RequestParam(value = "suppressLayouts", defaultValue = "0") int suppressLayouts,
                                  @RequestParam(value = "failFast", defaultValue = "0") int failFast) {

        String storageId = getDefaultStorageId(repositoryId);
        boolean checkRepository = checkRepository(storageId, repositoryId);
        if (!checkRepository) {
            return repositoryNotFound("source");
        }
        // 解析目标地址 目录地址必须是/开始
        if (!targetPath.startsWith("/")) {
            targetPath = "/" + targetPath;
        }
        String[] targetArr = targetPath.split("/");
        String targetRepositoryId = targetArr[1];
        String targetStorageId = getDefaultStorageId(targetRepositoryId);
        checkRepository = checkRepository(targetStorageId, targetRepositoryId);
        if (!checkRepository) {
            return repositoryNotFound("target");
        }
        log.info("Move repositoryId [{}] artifactPath [{}] to [{}] dry [{}] suppressLayouts [{}] failFast [{}]", repositoryId, artifactPath, targetPath, dry, suppressLayouts, failFast);
        ArtifactPromotion artifactPromotion = new ArtifactPromotion();

        artifactPromotion.setSrcStorageId(storageId);
        artifactPromotion.setSrcRepositoryId(repositoryId);
        artifactPromotion.setPath(artifactPath);
        artifactPromotion.setTargetPath(targetPath.replace(String.format("/%s/",targetRepositoryId), ""));

        TargetRepositoyDto targetRepositoyDto = new TargetRepositoyDto();
        targetRepositoyDto.setTargetStorageId(targetStorageId);
        targetRepositoyDto.setTargetRepositoryId(targetRepositoryId);
        artifactPromotion.setTargetRepositoyList(List.of(targetRepositoyDto));
        JSONObject jsonObject = new JSONObject();
        Map<String, Object> result = Maps.newHashMap();
        List<JSONObject> infoList = Lists.newArrayList();
        try {
            ResponseEntity responseEntity = artifactPromotionService.move(artifactPromotion);
            if (responseEntity.getStatusCode().value() == HttpStatus.OK.value()) {
                jsonObject.put("level", "info");
                jsonObject.put("message", "moving " + storageId + "/" + repositoryId + "/" + artifactPath + " to " + targetPath + " completed successfully");
                infoList.add(jsonObject);
                result.put("messages", infoList);
                return ResponseEntity.ok(result);
            } else {
                jsonObject.put("level", "error");
                jsonObject.put("message", "moving " + storageId + "/" + repositoryId + "/" + artifactPath + " to " + targetPath + " fail " + responseEntity.getStatusCode());
                infoList.add(jsonObject);
                result.put("messages", infoList);
                return ResponseEntity.ok(result);
            }
        } catch (Exception exception) {
            jsonObject.put("level", "error");
            jsonObject.put("message", "moving " + storageId + "/" + repositoryId + "/" + artifactPath + " to " + targetPath + " fail " + exception.getMessage());
            infoList.add(jsonObject);
            result.put("messages", infoList);
            return ResponseEntity.ok(result);
        }
    }
}
