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
package com.folib.components.thirdparty.client;

import cn.hutool.http.*;
import com.alibaba.fastjson.JSON;
import com.folib.components.thirdparty.client.domain.ServerResponse;
import com.folib.scanner.common.exception.BusinessException;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.MapUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.springframework.stereotype.Component;

import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.function.Consumer;

/**
 * @author veadan
 * @date 2024/4/22
 **/
@Slf4j
@Component
public class ThirdPartyClientComponent {

    /**
     * 发送请求返回单个对象
     *
     * @param method               方法类型
     * @param url                  地址
     * @param headers              headers
     * @param loadReqParamConsumer consumer
     * @param responseClass        返回类型
     * @param <T>                  返回类型
     * @return 结果
     */
    public <T> T doRequest(Method method, String url, Map<String, String> headers, Consumer<HttpRequest> loadReqParamConsumer, Class<T> responseClass) {
        try {
            String body = doRequest(method, url, headers, loadReqParamConsumer);
            return JSON.parseObject(body, responseClass);
        } catch (Exception e) {
            log.error("发起第三方请求失败（{}）", url, e);
            throw new BusinessException("发起第三方请求失败");
        }
    }

    /**
     * 发送请求返回数组对象
     *
     * @param method               方法类型
     * @param url                  地址
     * @param headers              headers
     * @param loadReqParamConsumer consumer
     * @param responseClass        返回类型
     * @param <T>                  返回类型
     * @return 结果
     */
    public <T> List<T> doRequestGetArray(Method method, String url, Map<String, String> headers, Consumer<HttpRequest> loadReqParamConsumer, Class<T> responseClass) {
        try {
            String body = doRequest(method, url, headers, loadReqParamConsumer);
            return JSON.parseArray(body, responseClass);
        } catch (Exception e) {
            log.error("发起第三方请求失败（{}）", url, e);
            throw new BusinessException("发起第三方请求失败");
        }
    }

    /**
     * 发送请求返回结果
     *
     * @param method               方法类型
     * @param url                  地址
     * @param headers              headers
     * @param loadReqParamConsumer consumer
     * @return 结果
     */
    private String doRequest(Method method, String url, Map<String, String> headers, Consumer<HttpRequest> loadReqParamConsumer) {
        try {
            ServerResponse serverResponse = doCommonRequest(method, url, headers, loadReqParamConsumer);
            if (HttpStatus.HTTP_OK != serverResponse.getCode()) {
                log.error("Response status error code [{}] url [{}] body [{}]", serverResponse.getCode(), url, serverResponse.getData());
                throw new BusinessException("发送第三方请求响应状态不正确");
            }
            return serverResponse.getData();
        } catch (Exception e) {
            log.error("Send request error url [{}] error [{}]", url, ExceptionUtils.getStackTrace(e));
            throw new BusinessException("发送第三方请求失败");
        }
    }

    /**
     * 发送请求返回结果
     *
     * @param method               方法类型
     * @param url                  地址
     * @param headers              headers
     * @param loadReqParamConsumer consumer
     * @return 结果
     */
    public ServerResponse doCommonRequest(Method method, String url, Map<String, String> headers, Consumer<HttpRequest> loadReqParamConsumer) {
        try {
            final HttpRequest request = HttpUtil.createRequest(method, url);
            if (Objects.nonNull(loadReqParamConsumer)) {
                loadReqParamConsumer.accept(request);
            }
            if (MapUtils.isNotEmpty(headers)) {
                request.headerMap(headers, true);
            }
            final HttpResponse response = request.execute();
            final String body = response.body();
            return ServerResponse.builder().data(body).code(response.getStatus()).build();
        } catch (Exception e) {
            log.error("Send request error url [{}] error [{}]", url, ExceptionUtils.getStackTrace(e));
            throw new BusinessException("发送第三方请求失败");
        }
    }
}
