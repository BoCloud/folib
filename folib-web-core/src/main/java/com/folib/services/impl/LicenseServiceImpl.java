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
package com.folib.services.impl;

import com.alibaba.fastjson.JSONObject;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.github.pagehelper.Page;
import com.github.pagehelper.PageHelper;
import com.google.common.collect.Maps;
import com.hazelcast.core.HazelcastInstance;
import com.folib.domain.license.LicenseBlackWhite;
import com.folib.entity.License;
import com.folib.forms.license.LicenseTableForm;
import com.folib.mapper.LicenseMapper;
import com.folib.scanner.common.msg.TableResultResponse;
import com.folib.service.ProxyRepositoryConnectionPoolConfigurationService;
import com.folib.services.LicenseService;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Lazy;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import javax.ws.rs.client.Client;
import javax.ws.rs.client.Invocation;
import javax.ws.rs.client.WebTarget;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import java.util.*;
import java.util.concurrent.TimeUnit;
import java.util.stream.Collectors;

/**
 * @author veadan
 **/
@Slf4j
@Service
public class LicenseServiceImpl implements LicenseService {

    private final static String LICENSE_CACHE = "LICENSE_CACHE";

    private final static String LICENSE_CACHE_KEY = "LICENSE_CACHE_KEY";

    @Autowired
    private LicenseMapper licenseMapper;

    @Autowired
    @Lazy
    private ProxyRepositoryConnectionPoolConfigurationService clientPool;

    @Autowired
    private HazelcastInstance hazelcastInstance;


    /**
     * 将字符串按照指定长度分割成字符串数组
     *
     * @param src
     * @param length
     * @return
     */
    public static String[] stringToStringArray(String src, int length) {
        int len = src.length();
        int startIndex = 0;
        int endIndex = length;
        int numChunks = (int) Math.ceil((double) len / length); // 计算需要的子字符串数量
        String[] chunks = new String[numChunks];
        for (int i = 0; i < numChunks; i++) {
            if (endIndex > len) {
                endIndex = len;
            }
            String substring = src.substring(startIndex, endIndex);
            chunks[i] = substring;
            startIndex = endIndex;
            endIndex += length;
        }
        return chunks;

    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void saveLicense(License license) {
        License dbLicense = selectOneLicense(license);
        if (Objects.nonNull(dbLicense)) {
            updateLicense(license);
        } else {
            licenseMapper.insert(license);
        }
        clearCache();
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void updateLicense(License license) {
        License dbLicense = selectOneLicense(license);
        if (Objects.nonNull(dbLicense)) {
            licenseMapper.update(license, Wrappers.<License>lambdaUpdate().eq(License::getLicenseId, dbLicense.getLicenseId()));
        }
        clearCache();
    }

    @Override
    public List<License> selectLicense(License license) {

        return licenseMapper.selectList(Wrappers.<License>lambdaQuery()
                .eq(Objects.nonNull(license) && license.getIsDeprecated()!=null,License::getIsDeprecated,license.getIsDeprecated())
                .orderByAsc(License::getId)
        );
    }

    @Override
    public License selectOneLicense(License license) {
        return licenseMapper.selectOne(Wrappers.<License>lambdaQuery().eq(License::getLicenseId, license.getLicenseId()));
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void updateLicenseUrl() {
        List<License> licenseList = selectLicense(License.builder().build());
        if (CollectionUtils.isEmpty(licenseList)) {
            return;
        }
        int size = licenseList.size();
        Map<String, String> headerMap = Maps.newHashMap();
        headerMap.put("X-Api-Key", "F3PvIyiAn8txkToGV8N8JfWphnfd57i8");
        Response response = null;
        String url = "", res = "", licenseUrl = "";
        JSONObject data = null;
        License license, updateLicense;
        for (int i = 0; i < size; i++) {
            license = licenseList.get(i);
            url = "http://192.168.5.8:8081/api/v1/license/%s";
            url = String.format(url, license.getLicenseId());
            log.info("[{}] 许可证id [{}] 请求地址 [{}] 当前第 [{}] 个，剩余 [{}] 个", this.getClass().getSimpleName(), license.getLicenseId(), url, (i + 1), size - (i + 1));
            try {
                response = doGet(url, headerMap);
                res = response.readEntity(String.class);
                log.info("[{}] 许可证id [{}] 请求地址 [{}] 请求响应 [{}] ", this.getClass().getSimpleName(), license.getLicenseId(), url, res);
                data = JSONObject.parseObject(res);
                licenseUrl = data.getString("seeAlso");
                log.info("[{}] 许可证id [{}] 请求地址 [{}] 许可证地址 [{}] ", this.getClass().getSimpleName(), license.getLicenseId(), url, licenseUrl);
                if (StringUtils.isNotBlank(licenseUrl)) {
                    licenseUrl = StringUtils.strip(licenseUrl, "[]");
                    updateLicense = License.builder().licenseId(license.getLicenseId()).licenseUrl(licenseUrl).build();
                    updateLicense(updateLicense);
                }
            } catch (Exception ex) {
                log.info("[{}] 许可证id [{}] 请求地址 [{}] 请求响应 [{}] 错误 [{}]", this.getClass().getSimpleName(), license.getLicenseId(), url, res, ExceptionUtils.getStackTrace(ex));
            } finally {
                if (Objects.nonNull(response)) {
                    response.close();
                }
            }
        }
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void updateContentCn() {
        List<License> licenseList = selectLicense(License.builder().build());
        if (CollectionUtils.isEmpty(licenseList)) {
            return;
        }
        int size = licenseList.size();
        License license, updateLicense;
        String contentCn = "";
        Long total = 0L;
        for (int i = 0; i < licenseList.size(); i++) {
            license = licenseList.get(i);
            log.info("[{}] 许可证id [{}] 当前第 [{}] 个，剩余 [{}] 个", this.getClass().getSimpleName(), license.getLicenseId(), (i + 1), size - (i + 1));
            try {
//                if (StringUtils.isNotBlank(license.getContent())) {
//                    log.info("[{}] 许可证id [{}] 当前第 [{}] 个，长度 [{}] 个", this.getClass().getSimpleName(), license.getLicenseId(), (i + 1), license.getContent().length());
//                    total = total + license.getContent().length();
//                }
                if (StringUtils.isNotBlank(license.getContent())) {
                    int length = license.getContent().length();
                    if (length > 2000) {
                        StringBuilder stringBuilder = new StringBuilder();
                        String[] arr = stringToStringArray(license.getContent(), 2000);
                        for (int j = 0; j < arr.length; j++) {
                            String content = arr[j];
                           // contentCn = Translate.translate(content);
                            stringBuilder.append(contentCn);
                        }
                        contentCn = stringBuilder.toString();
                    } else {
                        //contentCn = Translate.translate(license.getContent());
                    }
                }
                if (StringUtils.isNotBlank(contentCn)) {
                    updateLicense = License.builder().licenseId(license.getLicenseId()).contentCn(contentCn).build();
                    updateLicense(updateLicense);
                }
            } catch (Exception ex) {
                log.info("[{}] 许可证id [{}] 错误 [{}]", this.getClass().getSimpleName(), license.getLicenseId(), ExceptionUtils.getStackTrace(ex));
            }
        }
//        log.info("[{}] 许可证一共 [{}] 个字符", this.getClass().getSimpleName(), total);
    }

    @Override
    public TableResultResponse<LicenseTableForm> queryLicensePage(Integer page, Integer limit, String searchKeyword, String licenseId, Integer blackWhiteType) {
        if (Objects.isNull(page)) {
            page = 1;
        }
        if (Objects.isNull(limit)) {
            limit = 10;
        }
        Page<Object> result = PageHelper.startPage(page, limit);
        List<License> licenseList = licenseMapper.selectLicense(searchKeyword, licenseId, blackWhiteType, null, null);

        return new TableResultResponse<LicenseTableForm>(result.getTotal(), Optional.ofNullable(licenseList).orElse(Collections.emptyList()).stream().map(license -> {
            LicenseTableForm licenseTableForm = LicenseTableForm.builder().build();
            BeanUtils.copyProperties(license, licenseTableForm);
            return licenseTableForm;
        }).collect(Collectors.toList()));
    }

    @Override
    public List<LicenseTableForm> queryLicense(String searchKeyword, String licenseId, Integer blackWhiteType, Integer excludeBlackWhiteType) {
        List<License> licenseList = licenseMapper.selectLicense(searchKeyword, licenseId, blackWhiteType, excludeBlackWhiteType, null);
        return Optional.ofNullable(licenseList).orElse(Collections.emptyList()).stream().map(license -> {
            LicenseTableForm licenseTableForm = LicenseTableForm.builder().build();
            BeanUtils.copyProperties(license, licenseTableForm);
            return licenseTableForm;
        }).collect(Collectors.toList());
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void blackWhite(LicenseBlackWhite licenseBlackWhite) {
        License license = selectOneLicense(License.builder().licenseId(licenseBlackWhite.getLicenseId()).build());
        if (Objects.isNull(license)) {
            return;
        }
        licenseMapper.update(License.builder().blackWhiteType(licenseBlackWhite.getBlackWhiteType()).build(), Wrappers.<License>lambdaUpdate()
                .eq(License::getLicenseId, license.getLicenseId()));
        clearCache();
    }

    @Override
    public List<License> getLicenseCache() {
        List<License> licenses = getCache();
        if (CollectionUtils.isEmpty(licenses)) {
            licenses = licenseMapper.selectLicense(null, null, null, null, 0);
            putCache(licenses, 8);
        }
        return licenses;
    }

    /**
     * 发送get请求
     *
     * @param url       url
     * @param headerMap headerMap
     * @return 响应
     */
    private Response doGet(String url, Map<String, String> headerMap) {
        Client client = clientPool.getRestClient();
        WebTarget target = client.target(url);
        Invocation.Builder builder = target.request(MediaType.APPLICATION_JSON);
        for (Map.Entry<String, String> entry : headerMap.entrySet()) {
            builder = builder.header(entry.getKey(), entry.getValue());
        }
        return builder.get();
    }

    /**
     * 刷新缓存
     */
    private void clearCache() {
        Map<String, List<License>> hazelcastMap = hazelcastInstance.getMap(LICENSE_CACHE_KEY);
        hazelcastMap.remove(LICENSE_CACHE);
    }

    /**
     * 加入缓存
     *
     * @param cacheValue 缓存值
     * @param ttl        缓存时间，小时
     */
    private void putCache(List<License> cacheValue, long ttl) {
        if (CollectionUtils.isEmpty(cacheValue)) {
            return;
        }
        hazelcastInstance.getMap(LICENSE_CACHE_KEY).put(LICENSE_CACHE, cacheValue, ttl, TimeUnit.HOURS);
    }

    /**
     * 获取缓存
     *
     * @return 缓存值
     */
    private List<License> getCache() {
        try {
            Map<String, List<License>> hazelcastMap = hazelcastInstance.getMap(LICENSE_CACHE_KEY);
            return hazelcastMap.get(LICENSE_CACHE);
        } catch (Exception ex) {
            log.warn(ExceptionUtils.getStackTrace(ex));
            return null;
        }
    }
}
