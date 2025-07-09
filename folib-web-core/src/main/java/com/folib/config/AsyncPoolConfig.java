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
package com.folib.config;

import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.scheduling.concurrent.ThreadPoolTaskExecutor;

import javax.annotation.PreDestroy;
import java.util.Objects;
import java.util.concurrent.RejectedExecutionHandler;
import java.util.concurrent.ThreadPoolExecutor;

@Slf4j
@Configuration
public class AsyncPoolConfig {

    @Value("${folib.threadPool.async.corePoolSize}")
    private Integer asyncCorePoolSize;

    @Value("${folib.threadPool.async.maxPoolSize}")
    private Integer asyncMaxPoolSize;

    @Value("${folib.threadPool.async.queueCapacity}")
    private Integer asyncQueueCapacity;

    @Value("${folib.threadPool.async.keepAliveSeconds}")
    private Integer asyncKeepAliveSeconds;

    @Value("${folib.threadPool.async.threadNamePrefix}")
    private String asyncThreadNamePrefix;

    @Value("${folib.threadPool.async.awaitTerminationSeconds}")
    private Integer asyncAwaitTerminationSeconds;

    @Value("${folib.threadPool.asyncEventListener.corePoolSize}")
    private Integer asyncEventListenerCorePoolSize;

    @Value("${folib.threadPool.asyncEventListener.maxPoolSize}")
    private Integer asyncEventListenerMaxPoolSize;

    @Value("${folib.threadPool.asyncEventListener.queueCapacity}")
    private Integer asyncEventListenerQueueCapacity;

    @Value("${folib.threadPool.asyncEventListener.keepAliveSeconds}")
    private Integer asyncEventListenerKeepAliveSeconds;

    @Value("${folib.threadPool.asyncEventListener.threadNamePrefix}")
    private String asyncEventListenerThreadNamePrefix;

    @Value("${folib.threadPool.asyncEventListener.awaitTerminationSeconds}")
    private Integer asyncEventListenerAwaitTerminationSeconds;

    @Value("${folib.threadPool.asyncConfig.corePoolSize}")
    private Integer asyncConfigCorePoolSize;

    @Value("${folib.threadPool.asyncConfig.maxPoolSize}")
    private Integer asyncConfigMaxPoolSize;

    @Value("${folib.threadPool.asyncConfig.queueCapacity}")
    private Integer asyncConfigQueueCapacity;

    @Value("${folib.threadPool.asyncConfig.keepAliveSeconds}")
    private Integer asyncConfigKeepAliveSeconds;

    @Value("${folib.threadPool.asyncConfig.threadNamePrefix}")
    private String asyncConfigThreadNamePrefix;

    @Value("${folib.threadPool.asyncConfig.awaitTerminationSeconds}")
    private Integer asyncConfigAwaitTerminationSeconds;

    @Value("${folib.threadPool.asyncFetchRemotePackage.corePoolSize}")
    private Integer asyncFetchRemotePackageCorePoolSize;

    @Value("${folib.threadPool.asyncFetchRemotePackage.maxPoolSize}")
    private Integer asyncFetchRemotePackageMaxPoolSize;

    @Value("${folib.threadPool.asyncFetchRemotePackage.queueCapacity}")
    private Integer asyncFetchRemotePackageQueueCapacity;

    @Value("${folib.threadPool.asyncFetchRemotePackage.keepAliveSeconds}")
    private Integer asyncFetchRemotePackageKeepAliveSeconds;

    @Value("${folib.threadPool.asyncFetchRemotePackage.threadNamePrefix}")
    private String asyncFetchRemotePackageThreadNamePrefix;

    @Value("${folib.threadPool.asyncFetchRemotePackage.awaitTerminationSeconds}")
    private Integer asyncFetchRemotePackageAwaitTerminationSeconds;

    @Value("${folib.threadPool.asyncScan.corePoolSize}")
    private Integer asyncScanCorePoolSize;

    @Value("${folib.threadPool.asyncScan.maxPoolSize}")
    private Integer asyncScanMaxPoolSize;

    @Value("${folib.threadPool.asyncScan.queueCapacity}")
    private Integer asyncScanQueueCapacity;

    @Value("${folib.threadPool.asyncScan.keepAliveSeconds}")
    private Integer asyncScanKeepAliveSeconds;

    @Value("${folib.threadPool.asyncScan.threadNamePrefix}")
    private String asyncScanThreadNamePrefix;

    @Value("${folib.threadPool.asyncScan.awaitTerminationSeconds}")
    private Integer asyncScanAwaitTerminationSeconds;


    @Value("${folib.threadPool.asyncWsCommand.corePoolSize}")
    private Integer asyncWsCommandArtifactCorePoolSize;

    @Value("${folib.threadPool.asyncWsCommand.maxPoolSize}")
    private Integer asyncWsCommandArtifactMaxPoolSize;

    @Value("${folib.threadPool.asyncWsCommand.queueCapacity}")
    private Integer asyncWsCommandArtifactQueueCapacity;

    @Value("${folib.threadPool.asyncWsCommand.keepAliveSeconds}")
    private Integer asyncWsCommandArtifactKeepAliveSeconds;

    @Value("${folib.threadPool.asyncWsCommand.threadNamePrefix}")
    private String asyncWsCommandArtifactThreadNamePrefix;

    @Value("${folib.threadPool.asyncWsCommand.awaitTerminationSeconds}")
    private Integer asyncWsCommandArtifactAwaitTerminationSeconds;

    @Value("${folib.threadPool.asyncApiBrowse.corePoolSize:#{T(java.lang.Runtime).getRuntime().availableProcessors()}}")
    private Integer asyncApiBrowseArtifactCorePoolSize ;

    @Value("${folib.threadPool.asyncApiBrowse.maxPoolSize:#{T(java.lang.Runtime).getRuntime().availableProcessors()*2}}")
    private Integer asyncApiBrowseArtifactMaxPoolSize ;

    @Value("${folib.threadPool.asyncApiBrowse.queueCapacity}")
    private Integer asyncApiBrowseArtifactQueueCapacity;

    @Value("${folib.threadPool.asyncApiBrowse.keepAliveSeconds}")
    private Integer asyncApiBrowseArtifactKeepAliveSeconds;

    @Value("${folib.threadPool.asyncApiBrowse.threadNamePrefix}")
    private String asyncApiBrowseArtifactThreadNamePrefix;

    @Value("${folib.threadPool.asyncApiBrowse.awaitTerminationSeconds}")
    private Integer asyncApiBrowseArtifactAwaitTerminationSeconds;

    @Value("${folib.threadPool.asyncCopy.corePoolSize}")
    private Integer asyncCopyCorePoolSize;

    @Value("${folib.threadPool.asyncCopy.maxPoolSize}")
    private Integer asyncCopyMaxPoolSize;

    @Value("${folib.threadPool.asyncCopy.queueCapacity}")
    private Integer asyncCopyQueueCapacity;

    @Value("${folib.threadPool.asyncCopy.keepAliveSeconds}")
    private Integer asyncCopyKeepAliveSeconds;

    @Value("${folib.threadPool.asyncCopy.threadNamePrefix}")
    private String asyncCopyThreadNamePrefix;

    @Value("${folib.threadPool.asyncCopy.awaitTerminationSeconds}")
    private Integer asyncCopyAwaitTerminationSeconds;

    @Value("${folib.threadPool.asyncEventLog.corePoolSize}")
    private Integer asyncEventLogCorePoolSize;

    @Value("${folib.threadPool.asyncEventLog.maxPoolSize}")
    private Integer asyncEventLogMaxPoolSize;

    @Value("${folib.threadPool.asyncEventLog.queueCapacity}")
    private Integer asyncEventLogQueueCapacity;

    @Value("${folib.threadPool.asyncEventLog.keepAliveSeconds}")
    private Integer asyncEventLogKeepAliveSeconds;

    @Value("${folib.threadPool.asyncEventLog.threadNamePrefix}")
    private String asyncEventLogThreadNamePrefix;

    @Value("${folib.threadPool.asyncEventLog.awaitTerminationSeconds}")
    private Integer asyncEventLogAwaitTerminationSeconds;

    @Value("${folib.threadPool.asyncWsHeartbeat.corePoolSize}")
    private Integer asyncWsHeartbeatCorePoolSize;

    @Value("${folib.threadPool.asyncWsHeartbeat.maxPoolSize}")
    private Integer asyncWsHeartbeatMaxPoolSize;

    @Value("${folib.threadPool.asyncWsHeartbeat.queueCapacity}")
    private Integer asyncWsHeartbeatQueueCapacity;

    @Value("${folib.threadPool.asyncWsHeartbeat.keepAliveSeconds}")
    private Integer asyncWsHeartbeatKeepAliveSeconds;

    @Value("${folib.threadPool.asyncWsHeartbeat.threadNamePrefix}")
    private String asyncWsHeartbeatThreadNamePrefix;

    @Value("${folib.threadPool.asyncWsHeartbeat.awaitTerminationSeconds}")
    private Integer asyncWsHeartbeatAwaitTerminationSeconds;
    
    @Value("${folib.threadPool.asyncPromotion.corePoolSize}")
    private Integer asyncPromotionCorePoolSize;

    @Value("${folib.threadPool.asyncPromotion.maxPoolSize}")
    private Integer asyncPromotionMaxPoolSize;

    @Value("${folib.threadPool.asyncPromotion.queueCapacity}")
    private Integer asyncPromotionQueueCapacity;

    @Value("${folib.threadPool.asyncPromotion.keepAliveSeconds}")
    private Integer asyncPromotionKeepAliveSeconds;

    @Value("${folib.threadPool.asyncPromotion.threadNamePrefix}")
    private String asyncPromotionThreadNamePrefix;

    @Value("${folib.threadPool.asyncPromotion.awaitTerminationSeconds}")
    private Integer asyncPromotionAwaitTerminationSeconds;

    @Value("${folib.threadPool.asyncDeleteArtifact.corePoolSize}")
    private Integer asyncDeleteArtifactCorePoolSize;

    @Value("${folib.threadPool.asyncDeleteArtifact.maxPoolSize}")
    private Integer asyncDeleteArtifactMaxPoolSize;

    @Value("${folib.threadPool.asyncDeleteArtifact.queueCapacity}")
    private Integer asyncDeleteArtifactQueueCapacity;

    @Value("${folib.threadPool.asyncDeleteArtifact.keepAliveSeconds}")
    private Integer asyncDeleteArtifactKeepAliveSeconds;

    @Value("${folib.threadPool.asyncDeleteArtifact.threadNamePrefix}")
    private String asyncDeleteArtifactThreadNamePrefix;

    @Value("${folib.threadPool.asyncDeleteArtifact.awaitTerminationSeconds}")
    private Integer asyncDeleteArtifactAwaitTerminationSeconds;



    @Bean
    public ThreadPoolTaskExecutor asyncThreadPoolTaskExecutor() {
        return buildThreadPoolTaskExecutor(asyncCorePoolSize, asyncMaxPoolSize, asyncQueueCapacity, asyncKeepAliveSeconds, asyncThreadNamePrefix, asyncAwaitTerminationSeconds);
    }

    @Bean
    public ThreadPoolTaskExecutor asyncEventListenerExecutor() {
        return buildThreadPoolTaskExecutor(asyncEventListenerCorePoolSize, asyncEventListenerMaxPoolSize, asyncEventListenerQueueCapacity, asyncEventListenerKeepAliveSeconds, asyncEventListenerThreadNamePrefix, asyncEventListenerAwaitTerminationSeconds);
    }

    @Bean
    public ThreadPoolTaskExecutor asyncConfigThreadPoolExecutor() {
        return buildThreadPoolTaskExecutor(asyncConfigCorePoolSize, asyncConfigMaxPoolSize, asyncConfigQueueCapacity, asyncConfigKeepAliveSeconds, asyncConfigThreadNamePrefix, asyncConfigAwaitTerminationSeconds);
    }

    @Bean
    public ThreadPoolTaskExecutor asyncFetchRemotePackageThreadPoolTaskExecutor() {
        return buildThreadPoolTaskExecutor(
                asyncFetchRemotePackageCorePoolSize,
                asyncFetchRemotePackageMaxPoolSize,
                asyncFetchRemotePackageQueueCapacity,
                asyncFetchRemotePackageKeepAliveSeconds,
                asyncFetchRemotePackageThreadNamePrefix,
                asyncFetchRemotePackageAwaitTerminationSeconds);
    }

    @Bean
    public ThreadPoolTaskExecutor asyncScanThreadPoolTaskExecutor() {
        return buildThreadPoolTaskExecutor(
                asyncScanCorePoolSize,
                asyncScanMaxPoolSize,
                asyncScanQueueCapacity,
                asyncScanKeepAliveSeconds,
                asyncScanThreadNamePrefix,
                asyncScanAwaitTerminationSeconds,
                new ThreadPoolExecutor.DiscardPolicy());
    }

    @Bean
    public ThreadPoolTaskExecutor asyncWsCommandThreadPoolTaskExecutor() {
        return buildThreadPoolTaskExecutorV2(
                asyncWsCommandArtifactCorePoolSize,
                asyncWsCommandArtifactMaxPoolSize,
                asyncWsCommandArtifactQueueCapacity,
                asyncWsCommandArtifactKeepAliveSeconds,
                asyncWsCommandArtifactThreadNamePrefix,
                asyncWsCommandArtifactAwaitTerminationSeconds,
                null);
    }

    @Bean
    public ThreadPoolTaskExecutor asyncApiBrowseThreadPoolExecutor() {
        ThreadPoolTaskExecutor threadPoolTaskExecutor = new ThreadPoolTaskExecutor();
        threadPoolTaskExecutor.setRejectedExecutionHandler(new ThreadPoolExecutor.AbortPolicy());
        if (asyncApiBrowseArtifactCorePoolSize == null || asyncApiBrowseArtifactCorePoolSize == 0) {
            asyncApiBrowseArtifactCorePoolSize = Runtime.getRuntime().availableProcessors();
        }
        threadPoolTaskExecutor.setCorePoolSize(asyncApiBrowseArtifactCorePoolSize);
        if (asyncApiBrowseArtifactMaxPoolSize == null || asyncApiBrowseArtifactMaxPoolSize == 0) {
            asyncApiBrowseArtifactMaxPoolSize = Runtime.getRuntime().availableProcessors() * 2;
        }
        threadPoolTaskExecutor.setMaxPoolSize(asyncApiBrowseArtifactMaxPoolSize);
        threadPoolTaskExecutor.setQueueCapacity(asyncApiBrowseArtifactQueueCapacity);
        threadPoolTaskExecutor.setThreadNamePrefix(asyncApiBrowseArtifactThreadNamePrefix);
        threadPoolTaskExecutor.setKeepAliveSeconds(asyncApiBrowseArtifactKeepAliveSeconds);
        threadPoolTaskExecutor.setAwaitTerminationSeconds(asyncApiBrowseArtifactAwaitTerminationSeconds);
        threadPoolTaskExecutor.initialize();
        return threadPoolTaskExecutor;
    }

    @Bean
    public ThreadPoolTaskExecutor asyncCopyThreadPoolTaskExecutor() {
        return buildThreadPoolTaskExecutor(
                asyncCopyCorePoolSize,
                asyncCopyMaxPoolSize,
                asyncCopyQueueCapacity,
                asyncCopyKeepAliveSeconds,
                asyncCopyThreadNamePrefix,
                asyncCopyAwaitTerminationSeconds);
    }

    @Bean
    public ThreadPoolTaskExecutor asyncEventLogThreadPoolTaskExecutor() {
        return buildThreadPoolTaskExecutor(
                asyncEventLogCorePoolSize,
                asyncEventLogMaxPoolSize,
                asyncEventLogQueueCapacity,
                asyncEventLogKeepAliveSeconds,
                asyncEventLogThreadNamePrefix,
                asyncEventLogAwaitTerminationSeconds);
    }

    @Bean
    public ThreadPoolTaskExecutor asyncWsHeartbeatThreadPoolTaskExecutor() {
        return buildThreadPoolTaskExecutor(
                asyncWsHeartbeatCorePoolSize,
                asyncWsHeartbeatMaxPoolSize,
                asyncWsHeartbeatQueueCapacity,
                asyncWsHeartbeatKeepAliveSeconds,
                asyncWsHeartbeatThreadNamePrefix,
                asyncWsHeartbeatAwaitTerminationSeconds);
    }

    @Bean
    public ThreadPoolTaskExecutor asyncPromotionPoolTaskExecutor() {
        return buildThreadPoolTaskExecutor(
                asyncPromotionCorePoolSize,
                asyncPromotionMaxPoolSize,
                asyncPromotionQueueCapacity,
                asyncPromotionKeepAliveSeconds,
                asyncPromotionThreadNamePrefix,
                asyncPromotionAwaitTerminationSeconds);
    }

    @Bean
    public ThreadPoolTaskExecutor asyncCheckSumTaskExecutor() {
        return buildThreadPoolTaskExecutor(
                asyncPromotionCorePoolSize,
                asyncPromotionMaxPoolSize,
                asyncPromotionQueueCapacity,
                asyncPromotionKeepAliveSeconds,
                "asyncCheckSumTask",
                asyncPromotionAwaitTerminationSeconds);
    }

    @Bean
    public ThreadPoolTaskExecutor asyncDeleteArtifactTaskExecutor() {
        return buildThreadPoolTaskExecutor(
                asyncDeleteArtifactCorePoolSize,
                asyncDeleteArtifactMaxPoolSize,
                asyncDeleteArtifactQueueCapacity,
                asyncDeleteArtifactKeepAliveSeconds,
                asyncDeleteArtifactThreadNamePrefix,
                asyncDeleteArtifactAwaitTerminationSeconds);
    }

    /**
     * build ThreadPoolTaskExecutor
     *
     * @param corePoolSize            corePoolSize
     * @param maxPoolSize             maxPoolSize
     * @param queueCapacity           queueCapacity
     * @param keepAliveSeconds        keepAliveSeconds
     * @param threadNamePrefix        threadNamePrefix
     * @param awaitTerminationSeconds awaitTerminationSeconds
     * @return ThreadPoolTaskExecutor
     */
    private ThreadPoolTaskExecutor buildThreadPoolTaskExecutor(Integer corePoolSize, Integer maxPoolSize, Integer queueCapacity, Integer keepAliveSeconds, String threadNamePrefix, Integer awaitTerminationSeconds) {
        return buildThreadPoolTaskExecutor(corePoolSize, maxPoolSize, queueCapacity, keepAliveSeconds, threadNamePrefix, awaitTerminationSeconds, null);
    }

    /**
     * build ThreadPoolTaskExecutor
     *
     * @param corePoolSize             corePoolSize
     * @param maxPoolSize              maxPoolSize
     * @param queueCapacity            queueCapacity
     * @param keepAliveSeconds         keepAliveSeconds
     * @param threadNamePrefix         threadNamePrefix
     * @param awaitTerminationSeconds  awaitTerminationSeconds
     * @param rejectedExecutionHandler rejectedExecutionHandler
     * @return ThreadPoolTaskExecutor
     */
    private ThreadPoolTaskExecutor buildThreadPoolTaskExecutor(Integer corePoolSize, Integer maxPoolSize, Integer queueCapacity, Integer keepAliveSeconds, String threadNamePrefix, Integer awaitTerminationSeconds, RejectedExecutionHandler rejectedExecutionHandler) {
        ThreadPoolTaskExecutor executor = new ThreadPoolTaskExecutor();
        int availableCores = getAvailableCores();
        log.info("Current available cpu cores [{}]", availableCores);
        if (availableCores < 8) {
            availableCores = 8;
            log.info("Modify available cpu cores [{}]", availableCores);
        }
        if (corePoolSize > availableCores) {
            executor.setCorePoolSize(availableCores);
            executor.setMaxPoolSize(availableCores);
        } else {
            executor.setCorePoolSize(corePoolSize);
            executor.setMaxPoolSize(maxPoolSize);
        }
        Integer maxQueueCapacity = 100000000;
        if (queueCapacity > maxQueueCapacity) {
            queueCapacity = maxQueueCapacity;
        }
        executor.setQueueCapacity(queueCapacity);
        executor.setKeepAliveSeconds(keepAliveSeconds);
        executor.setThreadNamePrefix(threadNamePrefix);
        executor.setWaitForTasksToCompleteOnShutdown(true);
        executor.setAwaitTerminationSeconds(awaitTerminationSeconds);
        if (Objects.isNull(rejectedExecutionHandler)) {
            rejectedExecutionHandler = new ThreadPoolExecutor.CallerRunsPolicy();
        }
        executor.setRejectedExecutionHandler(rejectedExecutionHandler);
        executor.initialize();
        log.info("Thread pool name [{}] core size [{}] max size [{}] queue capacity [{}]", executor.getThreadNamePrefix(), executor.getCorePoolSize(), executor.getMaxPoolSize(), queueCapacity);
        return executor;
    }

    private ThreadPoolTaskExecutor buildThreadPoolTaskExecutorV2(Integer corePoolSize, Integer maxPoolSize, Integer queueCapacity, Integer keepAliveSeconds, String threadNamePrefix, Integer awaitTerminationSeconds, RejectedExecutionHandler rejectedExecutionHandler) {
        ThreadPoolTaskExecutor executor = new ThreadPoolTaskExecutor();

        executor.setCorePoolSize(corePoolSize);
        executor.setMaxPoolSize(maxPoolSize);
        Integer maxQueueCapacity = 100000000;
        if (queueCapacity > maxQueueCapacity) {
            queueCapacity = maxQueueCapacity;
        }
        executor.setQueueCapacity(queueCapacity);
        executor.setKeepAliveSeconds(keepAliveSeconds);
        executor.setThreadNamePrefix(threadNamePrefix);
        executor.setWaitForTasksToCompleteOnShutdown(true);
        executor.setAwaitTerminationSeconds(awaitTerminationSeconds);
        if (Objects.isNull(rejectedExecutionHandler)) {
            rejectedExecutionHandler = new ThreadPoolExecutor.CallerRunsPolicy();
        }
        executor.setRejectedExecutionHandler(rejectedExecutionHandler);
        executor.initialize();
        log.info("Thread pool name [{}] core size [{}] max size [{}] queue capacity [{}]", executor.getThreadNamePrefix(), executor.getCorePoolSize(), executor.getMaxPoolSize(), queueCapacity);
        return executor;
    }

    private int getAvailableCores() {
        return Runtime.getRuntime().availableProcessors();
    }
}

