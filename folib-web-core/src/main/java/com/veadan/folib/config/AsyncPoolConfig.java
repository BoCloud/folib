package com.veadan.folib.config;

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
        return buildThreadPoolTaskExecutor(
                asyncWsCommandArtifactCorePoolSize,
                asyncWsCommandArtifactMaxPoolSize,
                asyncWsCommandArtifactQueueCapacity,
                asyncWsCommandArtifactKeepAliveSeconds,
                asyncWsCommandArtifactThreadNamePrefix,
                asyncWsCommandArtifactAwaitTerminationSeconds);
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
                2,
                2,
                60,
                asyncEventLogKeepAliveSeconds,
                asyncEventLogThreadNamePrefix,
                asyncEventLogAwaitTerminationSeconds);
    }

    @PreDestroy
    public void shutdown() {
        asyncThreadPoolTaskExecutor().shutdown();
        asyncEventListenerExecutor().shutdown();
        asyncConfigThreadPoolExecutor().shutdown();
        asyncFetchRemotePackageThreadPoolTaskExecutor().shutdown();
        asyncScanThreadPoolTaskExecutor().shutdown();
        asyncWsCommandThreadPoolTaskExecutor().shutdown();
        asyncCopyThreadPoolTaskExecutor().shutdown();
        asyncEventLogThreadPoolTaskExecutor().shutdown();
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

    private int getAvailableCores() {
        return Runtime.getRuntime().availableProcessors();
    }
}

