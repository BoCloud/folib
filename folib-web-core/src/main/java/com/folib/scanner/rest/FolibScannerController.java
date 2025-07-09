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

import com.alibaba.fastjson.JSON;
import com.alibaba.fastjson.JSONArray;
import com.alibaba.fastjson.JSONObject;
import com.folib.scanner.biz.FolibScannerBiz;
import com.folib.scanner.common.msg.ObjectRestResponse;
import com.folib.scanner.common.msg.TableResultResponse;
import com.folib.scanner.entity.FolibScanner;
import com.folib.scanner.entity.FolibScannerDockerTableVO;
import com.folib.scanner.entity.SeverityVO;
import com.folib.scanner.mapper.FolibScannerMapper;
import com.folib.scanner.service.SbomScannerService;
import com.folib.utils.UserUtils;
import io.swagger.annotations.Api;
import jakarta.annotation.Resource;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import java.util.Map;

@RestController
@RequestMapping("/api/folibScanner")
@Api(tags = "")
public class FolibScannerController  {

    @Autowired
    private SbomScannerService scanService;
    @Resource
    private FolibScannerMapper folibScannerMapper;
    @Autowired
    private FolibScannerBiz folibScannerBiz;


    @GetMapping("/update")
    @PreAuthorize("hasAuthority('ADMIN')")
    public ObjectRestResponse updateDb(String cron) {
        scanService.vulnerabilityRefreshData(UserUtils.getUsername(), cron);
        return new ObjectRestResponse(true, "更新中");
    }

    @GetMapping("/scan")
    @PreAuthorize("hasAuthority('ADMIN')")
    public ObjectRestResponse scan(String cron) {
        scanService.artifactScan(UserUtils.getUsername(), cron);
        return new ObjectRestResponse(true, "");
    }

    @GetMapping("/getCount")
    public ObjectRestResponse getCount() {
        JSONObject object = new JSONObject();
        object.put("denpendencyCount", folibScannerBiz.getScanSum());
        object.put("totalCount", folibScannerBiz.getTotalSum());
        return new ObjectRestResponse(true, object, "获取数据成功");
    }

    @GetMapping("/getScannerSumDifVoList")
    public ObjectRestResponse getScannerSumDifVoList() {
        return new ObjectRestResponse(true, folibScannerBiz.getScannerSumDifVoList(), "获取数据成功");
    }

    @GetMapping("/weekDayCount")
    public ObjectRestResponse weekDayCount() {
        return new ObjectRestResponse(true, folibScannerBiz.weekDayCount(), "获取数据成功");
    }

    @GetMapping("/mounthDayCount")
    public ObjectRestResponse mounthDayCount() {
        return new ObjectRestResponse(true, folibScannerBiz.mounthDayCount(), "获取数据成功");
    }

    @GetMapping("/folibScannerGetOne")
    public ObjectRestResponse folibScannerGetOne(@RequestParam("id") String id) {
        FolibScanner folibScanner = folibScannerBiz.selectById(id);
        JSONArray jsonArray = JSON.parseArray(folibScanner.getReport());
        return new ObjectRestResponse<>(true, jsonArray, "成功");
    }

    /**
     * 获取制品的漏洞严重程度信息
     *
     * @param id    制品id
     * @param fuzzy 模糊匹配 0 否 1 是
     * @return 制品的漏洞严重程度信息
     */
    @GetMapping("/severity")
    public ObjectRestResponse<SeverityVO> severity(@RequestParam("id") String id, @RequestParam(name = "fuzzy", required = false, defaultValue = "0") Integer fuzzy) {
        return new ObjectRestResponse<SeverityVO>(true, folibScannerBiz.severity(id, fuzzy), "成功");
    }

    /**
     * 查询docker布局扫描报告
     *
     * @param params 参数
     * @return docker布局扫描报告
     */
    @GetMapping(value = "/dockerPage")
    public TableResultResponse<FolibScannerDockerTableVO> dockerPage(@RequestParam(required = false) Map<String, Object> params) {
        return folibScannerBiz.dockerPage(params);
    }

}
