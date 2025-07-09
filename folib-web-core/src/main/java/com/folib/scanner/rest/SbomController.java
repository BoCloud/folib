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
package com.folib.scanner.rest;

import com.folib.scanner.analyze.AnalyzeService;
import com.folib.scanner.analyze.SbomAnalyzeServer;
import com.folib.scanner.task.AnalyzeSbomTask;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang.exception.ExceptionUtils;
import org.cyclonedx.model.Bom;
import org.cyclonedx.parsers.JsonParser;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.multipart.MultipartFile;

import javax.inject.Inject;
import java.util.HashMap;
import java.util.Map;


@Slf4j
@RestController
@RequestMapping(value = "/api/sca")
@Api(description = "sbom 分析", tags = "sbom")
public class SbomController {

    @Inject
    private SbomAnalyzeServer sbomAnalsisServer;
    @Inject
    private AnalyzeService analyzeService;

    @ApiOperation(value = " sbom cyclonedx json 分析", notes = "")
    @PostMapping(value = "/sbom/cyclonedx", consumes = "multipart/form-data", produces = "application/json")
    public ResponseEntity<?> analysisCyclonedx(
            @RequestPart(required = false) MultipartFile sbomFile,
            @RequestParam(required = true) int code,
            @RequestParam(required = false) String projectId,
            @RequestParam(required = true) String taskName,
            @RequestParam(required = false) String message
            ) throws Exception {
        Bom bom;
        boolean success;
        if ((code==200 && sbomFile != null)) {
            success = true;
            JsonParser jsonParser = new JsonParser();
            bom = jsonParser.parse(sbomFile.getInputStream());
        } else {
            bom = null;
            success = false;
        }
        AnalyzeSbomTask task = new AnalyzeSbomTask(1, taskName, () -> {
            try {
                sbomAnalsisServer.analyzeCycloneDx(bom, success, projectId, message, code, taskName);
            }catch (Exception e){
                log.error("sbom scanner error  code:{} projectId:{} taskName:{} message:{}",code, projectId, taskName, message);
                log.error("sbom scanner error:{}", ExceptionUtils.getStackTrace(e));
            }
        });
        analyzeService.addTask(task);
        return ResponseEntity.ok("susses");
    }
    @ApiOperation(value = "获取制品分析配置", notes = "")
    @GetMapping(value = "/getAnalyzeConfig", produces = "application/json")
    public ResponseEntity<?> getAnalyzeConfig(){
        boolean enableAnalysis = Boolean.parseBoolean(System.getenv("folib.enableAnalysis"));
        Map<String, Object> config = new HashMap<>();
        config.put("enable",enableAnalysis);
        return ResponseEntity.ok(config);
    }

}
