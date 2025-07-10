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

import com.folib.entity.Dict;
import com.folib.forms.dict.DictForm;
import com.folib.scanner.common.exception.BusinessException;
import com.folib.scanner.common.msg.TableResultResponse;
import com.folib.services.DictService;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import javax.inject.Inject;
import java.util.List;
import java.util.Objects;

/**
 * @author veadan
 */
@Slf4j
@RestController
@RequestMapping("/api/dict")
@Api(description = "字典管理", tags = "字典管理")
public class DictController extends BaseController {

    @Inject
    private DictService dictService;

    @ApiOperation(value = "查询最新的单个字典信息")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "OK")})
    @PreAuthorize("authenticated")
    @GetMapping(value = "/single")
    public ResponseEntity<Dict> getDict(Dict dict) {
        Dict dictData = dictService.selectLatestOneDict(dict);
        return ResponseEntity.ok(dictData);
    }

    @ApiOperation(value = "更新单个字典信息")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "OK")})
    @PreAuthorize("authenticated")
    @PostMapping(value = "/single")
    public ResponseEntity<String> updateDict(@RequestBody DictForm dict) {
        if (Objects.isNull(dict.getId()) && StringUtils.isBlank(dict.getDictKey())) {
            throw new BusinessException("参数错误");
        }
        dictService.updateDict(dict);
        return ResponseEntity.ok("ok");
    }

    @ApiOperation(value = "查询最新的字典列表信息")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "OK")})
    @PreAuthorize("authenticated")
    @GetMapping(value = "/list")
    public ResponseEntity<List<Dict>> dictList(Dict dict) {
        return ResponseEntity.ok(dictService.selectLatestListDict(dict));
    }

    @ApiOperation(value = "删除字典信息")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "OK")})
    @PreAuthorize("authenticated")
    @DeleteMapping(value = "/delete")
    public ResponseEntity<String> delete(@RequestParam("id") Long id) {
        dictService.deleteDictById(id);
        return ResponseEntity.ok("ok");
    }

    @ApiOperation(value = "获取所有系统参数字典")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "OK")})
    @PreAuthorize("authenticated")
    @GetMapping(value = "/system")
    public TableResultResponse<Dict> systemDict(@RequestParam(name = "page", defaultValue = "1") Integer page, @RequestParam(name = "limit", defaultValue = "10") Integer limit, @RequestParam(required = false) String dictKey) {
        return dictService.getSystemDict(page, limit, dictKey);
    }
}
