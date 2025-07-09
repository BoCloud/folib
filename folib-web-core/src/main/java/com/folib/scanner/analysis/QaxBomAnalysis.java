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
package com.folib.scanner.analysis;

import com.folib.domain.Artifact;
import com.folib.enums.SafeLevelEnum;
import com.folib.providers.io.RepositoryPath;
import com.folib.providers.io.RepositoryPathResolver;
import com.folib.services.ArtifactService;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.apache.http.HttpEntity;
import org.apache.http.client.methods.CloseableHttpResponse;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.conn.ssl.NoopHostnameVerifier;
import org.apache.http.conn.ssl.SSLConnectionSocketFactory;
import org.apache.http.conn.ssl.TrustStrategy;
import org.apache.http.entity.ContentType;
import org.apache.http.entity.mime.MultipartEntityBuilder;
import org.apache.http.impl.client.CloseableHttpClient;
import org.apache.http.impl.client.HttpClients;
import org.apache.http.ssl.SSLContexts;
import org.apache.http.util.EntityUtils;
import org.springframework.context.annotation.Lazy;
import org.springframework.stereotype.Service;

import javax.inject.Inject;
import javax.net.ssl.SSLContext;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Optional;
import java.util.concurrent.CompletableFuture;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

@Slf4j
@Service(AnalysisConstant.QAX)
public class QaxBomAnalysis implements BomAnalysis {

    @Inject
    @Lazy
    private ArtifactService artifactService;
    @Inject
    @Lazy
    private RepositoryPathResolver repositoryPathResolver;
    @Inject
    @Lazy
    private QaxProperties qaxProperties;

    /**
     * 分析bom CycloneDXJson
     *
     * @param taskName 任务名称
     * @param filePath bom 文件路径
     */
    @Override
    public void analysisCycloneDXJson(final String taskName, RepositoryPath filePath) {
        if(qaxProperties.getEnable()){
            CompletableFuture.runAsync(() -> {
                try {
                    runAsyncAnalysis(taskName, filePath);
                } catch (Exception e) {
                    log.error("taskName:{} analysisCycloneDXJson分析异常:{}", taskName, ExceptionUtils.getStackTrace(e));
                    throw new RuntimeException(e);
                }
            }).handle((result, throwable) -> {
                if (throwable != null) {
                    Optional<Artifact> optionalArtifact = null;
                    try {
                        optionalArtifact = Optional.ofNullable(getArtifact(taskName));
                    } catch (IOException e) {
                        throw new RuntimeException(e);
                    }
                    if(optionalArtifact.isPresent()){
                        optionalArtifact.get().setSafeLevel(SafeLevelEnum.SCAN_FAIL.getLevel());
                        artifactService.saveOrUpdateArtifact( optionalArtifact.get());
                    }
                    log.error("Async task failed with exception", throwable);
                } else {
                    log.info("taskName:{} analysisCycloneDXJson分析完成", taskName);
                }
                return null;
            });
        }

    }

    public void runAsyncAnalysis(String taskName, RepositoryPath filePath) throws Exception {
        log.info("taskName:{} qax开始分析", taskName);
        final HttpPost httpPost = new HttpPost(qaxProperties.getBaseUrl());
        httpPost.setHeader("Authorization", getAuthorization());

        Path bomPath = Paths.get(filePath.toString());
        // 构建MultipartEntity，添加表单参数和文件
        MultipartEntityBuilder builder = MultipartEntityBuilder.create();
        builder.setContentType(ContentType.MULTIPART_FORM_DATA);
        // 添加文本参数
        builder.addTextBody("projectId", qaxProperties.getProjectId(), ContentType.TEXT_PLAIN);
        builder.addTextBody("taskName", taskName, ContentType.TEXT_PLAIN);
        builder.addTextBody("tags", qaxProperties.getTags(), ContentType.TEXT_PLAIN);
        try (InputStream fileStream = Files.newInputStream(bomPath)) { // 确保流资源自动关闭
            // 添加文件参数
            // 添加文件参数
            builder.addBinaryBody(
                    "file", // 表单字段名
                    fileStream, // 文件对象
                    ContentType.APPLICATION_OCTET_STREAM, // 内容类型
                    bomPath.getFileName().toString() // 文件名
            );
            // 构建请求实体
            HttpEntity multipartEntity = builder.build();
            httpPost.setEntity(multipartEntity);
            try (final CloseableHttpResponse response = createUnsafeHttpClient().execute(httpPost)) {
                log.info("taskName:{} qax请求成功", taskName);
                HttpEntity responseEntity = response.getEntity();
                // 将响应内容转换为字符串
                String result = EntityUtils.toString(responseEntity);
                log.info("qax 响应状态:{} ",response.getStatusLine());
                log.info("qax响应内容: {}",result);
                if(response.getStatusLine().getStatusCode() == 200){
                    log.info("qax请求成功 taskName:{} ", taskName);
                }else {
                    log.error("qax请求失败: taskName:{}", taskName);
                }

                // 确保响应实体被完全消费
                EntityUtils.consume(responseEntity);
            }
        } catch (IOException e) {
            // 规范异常日志记录
            log.error("taskName:{} qax请求异常", taskName, e);
            throw e; // 保持异常传播
        } catch (Exception e) {
            log.error("taskName:{} 发生未预期异常", taskName, e);
            throw new RuntimeException("处理请求时发生异常", e);
        }
    }

    private String base64Encode(String str) {
        return org.apache.commons.codec.binary.Base64.encodeBase64String(str.getBytes());
    }

    public String getAuthorization() {
        String token = qaxProperties.getPrivateToken();
        if (token != null) {
            return String.format("Private-Token %s",token);
        }
        if (qaxProperties.getUsername() != null && qaxProperties.getPassword() != null) {
            return "Basic " + base64Encode(String.join(":", qaxProperties.getUsername(), qaxProperties.getPassword()));
        }
        return null;
    }

    // 创建一个跳过 SSL 验证的 HttpClient
    private CloseableHttpClient createUnsafeHttpClient() throws Exception {
        // 信任所有证书
        TrustStrategy trustAllStrategy = (cert, authType) -> true;
        SSLContext sslContext = SSLContexts.custom()
                .loadTrustMaterial(null, trustAllStrategy)
                .build();

        // 允许所有主机名
        SSLConnectionSocketFactory sslSocketFactory = new SSLConnectionSocketFactory(
                sslContext,
                NoopHostnameVerifier.INSTANCE
        );

        return HttpClients.custom()
                .setSSLSocketFactory(sslSocketFactory)
                .build();
    }

    public Artifact getArtifact(String bomId) throws IOException {

        String regex = "^(?<storageId>[^:]+):(?<repositoryId>[^:]+):(?<path>.+)$";
        Pattern pattern = Pattern.compile(regex);
        Matcher matcher = pattern.matcher(bomId);
        String storageId = "", repositoryId = "";
        String path = null;
        if (matcher.matches()) {
            storageId = matcher.group("storageId");
            repositoryId = matcher.group("repositoryId");
            path = matcher.group("path");
        } else {
            throw new RuntimeException("sbom artifact UUID:" + bomId + "is not exist");
        }
        RepositoryPath repositoryPath = repositoryPathResolver.resolve(storageId, repositoryId, path);
        return artifactService.findArtifact(repositoryPath, false);
    }
}
