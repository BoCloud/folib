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
package com.folib.controllers;

import com.folib.domain.license.LicenseBlackWhite;
import com.folib.entity.License;
import com.folib.forms.license.LicenseTableForm;
import com.folib.scanner.common.msg.TableResultResponse;
import com.folib.services.LicenseService;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;
import org.springframework.beans.BeanUtils;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

import javax.inject.Inject;
import java.util.List;
import java.util.Objects;

/**
 * @author veadan
 */
@RestController
@RequestMapping("/api/license")
@Api(description = "license证书管理",tags = "license证书管理")
public class LicenseController extends BaseController {

    @Inject
    private LicenseService licenseService;

    @ApiOperation(value = "查询license分页列表", response = LicenseTableForm.class)
    @ApiResponses(value = {@ApiResponse(code = 200, message = "OK")})
    @PreAuthorize("hasAuthority('LICENSES_VIEW')")
    @GetMapping(value = "/page")
    public TableResultResponse<LicenseTableForm> page(@RequestParam(name = "page", required = false) Integer page,
                                                      @RequestParam(name = "limit", required = false) Integer limit,
                                                      @RequestParam(name = "searchKeyword", required = false) String searchKeyword,
                                                      @RequestParam(name = "licenseId", required = false) String licenseId,
                                                      @RequestParam(name = "blackWhiteType", required = false) Integer blackWhiteType) {
        return licenseService.queryLicensePage(page, limit, searchKeyword, licenseId, blackWhiteType);
    }

    @ApiOperation(value = "查询license列表", response = LicenseTableForm.class)
    @ApiResponses(value = {@ApiResponse(code = 200, message = "OK")})
    @PreAuthorize("hasAuthority('LICENSES_VIEW')")
    @GetMapping(value = "/list")
    public ResponseEntity<List<LicenseTableForm>> list(@RequestParam(name = "searchKeyword", required = false) String searchKeyword,
                                                       @RequestParam(name = "licenseId", required = false) String licenseId,
                                                       @RequestParam(name = "blackWhiteType", required = false) Integer blackWhiteType,
                                                       @RequestParam(name = "excludeBlackWhiteType", required = false) Integer excludeBlackWhiteType) {
        return ResponseEntity.ok(licenseService.queryLicense(searchKeyword, licenseId, blackWhiteType, excludeBlackWhiteType));
    }

    @ApiOperation(value = "查询license信息", response = LicenseTableForm.class)
    @ApiResponses(value = {@ApiResponse(code = 200, message = "OK")})
    @PreAuthorize("hasAuthority('LICENSES_VIEW')")
    @GetMapping(value = "/detail/{licenseId}")
    public ResponseEntity<LicenseTableForm> licenseInfo(@PathVariable(name = "licenseId") String licenseId) {
        License license = licenseService.selectOneLicense(License.builder().licenseId(licenseId).build());
        LicenseTableForm licenseTableForm = null;
        if (Objects.nonNull(license)) {
            licenseTableForm = LicenseTableForm.builder().build();
            BeanUtils.copyProperties(license, licenseTableForm);
        }
        return ResponseEntity.ok(licenseTableForm);
    }

    @ApiOperation(value = "设置黑白名单")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "OK")})
    @PreAuthorize("hasAuthority('ADMIN')")
    @PostMapping(value = "/blackWhite")
    public ResponseEntity<String> blackWhite(@RequestBody @Validated LicenseBlackWhite licenseBlackWhite) {
        License license = licenseService.selectOneLicense(License.builder().licenseId(licenseBlackWhite.getLicenseId()).build());
        if (Objects.isNull(license)) {
            return ResponseEntity.status(HttpStatus.NOT_FOUND).build();
        }
        licenseService.blackWhite(licenseBlackWhite);
        return ResponseEntity.ok("ok");
    }
}
