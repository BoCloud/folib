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
package com.folib.promotion;

import cn.hutool.extra.spring.SpringUtil;
import com.folib.components.security.SecurityComponent;
import com.folib.service.ProxyRepositoryConnectionPoolConfigurationService;
import com.folib.storage.repository.Repository;
import lombok.Data;
import lombok.extern.slf4j.Slf4j;
import org.apache.http.HttpEntity;
import org.apache.http.entity.ContentType;
import org.apache.http.entity.mime.HttpMultipartMode;
import org.apache.http.entity.mime.MultipartEntityBuilder;

import javax.ws.rs.client.Client;
import javax.ws.rs.client.Entity;
import javax.ws.rs.client.Invocation;
import javax.ws.rs.client.WebTarget;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import java.io.File;
import java.io.InputStream;
import java.nio.charset.Charset;
import java.nio.file.Files;
import java.util.concurrent.Callable;

@Data
@Slf4j
public class NodeOptionPushTask implements Callable<String> {

    /**
     * 源仓库
     */
    private Repository srcRepository;

    /**
     * 目标路径
     */
    private String URl;

    /**
     * 源路径下文件
     */
    private File file;

    private ProxyRepositoryConnectionPoolConfigurationService clientPool;

    private PromotionUtil promotionUtil;

    private SecurityComponent securityComponent;

    public NodeOptionPushTask() {
    }

    public NodeOptionPushTask(Repository srcRepository, String URl, File file) {
        this.srcRepository = srcRepository;
        this.URl = URl;
        this.file = file;
        this.clientPool = SpringUtil.getBean(ProxyRepositoryConnectionPoolConfigurationService.class);
        this.promotionUtil = SpringUtil.getBean(PromotionUtil.class);
        this.securityComponent = SpringUtil.getBean(SecurityComponent.class);
    }

    @Override
    public String call() {
        String rs = "";

        Client client = null;
        Response response = null;
        try (InputStream inputStream = Files.newInputStream(file.toPath())) {
            client = clientPool.getRestClient();
            WebTarget target = client.target(getURl());
            MultipartEntityBuilder multipartEntityBuilder = MultipartEntityBuilder.create()
                    .setMode(HttpMultipartMode.BROWSER_COMPATIBLE);

            multipartEntityBuilder.setCharset(Charset.forName("UTF-8"));
            multipartEntityBuilder.addTextBody("repository", "[]");
            //如果以inputStream形式上传文件，接收文件上传的接口可能会限制content-type为二进制：ContentType.DEFAULT_BINARY，否则为ContentType.MULTIPART_FORM_DATA
            multipartEntityBuilder.addBinaryBody("file", inputStream, ContentType.DEFAULT_BINARY, file.getName());
            HttpEntity httpEntity = multipartEntityBuilder.build();
            Invocation.Builder builder = target.request();
            securityComponent.securityTokenHeader(builder);
            response = builder.put(Entity.entity(httpEntity, MediaType.APPLICATION_JSON));
            if (response.getStatus() > 210) {
                log.error("Push artifact error {}", getURl());
                throw new RuntimeException("Failed with HTTP error code : " + response.getStatus());
            }

        } catch (Exception e) {
            log.error("NodeOptionPushTask error {}: ", e.getMessage());
            rs = e.getMessage();
        } finally {
            if (null != response) {
                response.close();
            }
            if (null != client) {
                client.close();
            }
        }
        return rs;
    }

}
