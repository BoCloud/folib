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

import com.folib.scanner.common.msg.ObjectRestResponse;
import com.folib.scanner.entity.ScanRules;
import com.folib.scanner.service.ScanRulesService;
import io.swagger.annotations.Api;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

@RestController
@RequestMapping("/api/scanRules")
@Api(tags = "规则扫描控制器", description = "规则扫描控制器")
public class ScanRulesController  {

    @Autowired
    private ScanRulesService scanRulesService;

    @PostMapping("/insertOrUpdate")
    public ObjectRestResponse insertOrUpdate(@RequestBody ScanRules scanRules) {
        scanRulesService.saveOrUpdateScanRules(scanRules);
        return new ObjectRestResponse(true, "更新成功");
    }

    @GetMapping("/queryBomOnScan")
    public ResponseEntity queryBomOnScan() {
        return ResponseEntity.ok(scanRulesService.queryBomOnScanList());
    }

    @GetMapping("/queryBomOnScanTree")
    public ResponseEntity queryBomOnScanTree() {
        return ResponseEntity.ok(scanRulesService.queryBomOnScanTree());
    }

    @GetMapping("/queryOnScanTree")
    public ResponseEntity queryOnScanTree() {
        return ResponseEntity.ok(scanRulesService.queryOnScanTree());
    }

    @GetMapping("/{scanId}")
    public ResponseEntity queryOnScan(@PathVariable("scanId") String scanId) {
        ObjectRestResponse<ScanRules> entityObjectRestResponse = new ObjectRestResponse<>();
        ScanRules rules = scanRulesService.findByScanId(scanId);
        entityObjectRestResponse.data(rules);
        entityObjectRestResponse.setRel(rules != null);
        return ResponseEntity.ok(entityObjectRestResponse);
    }
}
