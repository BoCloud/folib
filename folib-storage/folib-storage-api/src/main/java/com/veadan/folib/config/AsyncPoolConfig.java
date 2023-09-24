package com.veadan.folib.config;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.scheduling.concurrent.ThreadPoolTaskExecutor;

import java.util.concurrent.ThreadPoolExecutor;

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

    @Value("${folib.threadPool.asyncStorage.corePoolSize}")
    private Integer asyncStorageCorePoolSize;

    @Value("${folib.threadPool.asyncStorage.maxPoolSize}")
    private Integer asyncStorageMaxPoolSize;

    @Value("${folib.threadPool.asyncStorage.queueCapacity}")
    private Integer asyncStorageQueueCapacity;

    @Value("${folib.threadPool.asyncStorage.keepAliveSeconds}")
    private Integer asyncStorageKeepAliveSeconds;

    @Value("${folib.threadPool.asyncStorage.threadNamePrefix}")
    private String asyncStorageThreadNamePrefix;

    @Value("${folib.threadPool.asyncStorage.awaitTerminationSeconds}")
    private Integer asyncStorageAwaitTerminationSeconds;

    @Value("${folib.threadPool.asyncRepository.corePoolSize}")
    private Integer asyncRepositoryCorePoolSize;

    @Value("${folib.threadPool.asyncRepository.maxPoolSize}")
    private Integer asyncRepositoryMaxPoolSize;

    @Value("${folib.threadPool.asyncRepository.queueCapacity}")
    private Integer asyncRepositoryQueueCapacity;

    @Value("${folib.threadPool.asyncRepository.keepAliveSeconds}")
    private Integer asyncRepositoryKeepAliveSeconds;

    @Value("${folib.threadPool.asyncRepository.threadNamePrefix}")
    private String asyncRepositoryThreadNamePrefix;

    @Value("${folib.threadPool.asyncRepository.awaitTerminationSeconds}")
    private Integer asyncRepositoryAwaitTerminationSeconds;

    @Value("${folib.threadPool.asyncSecurityPolicy.corePoolSize}")
    private Integer asyncSecurityPolicyCorePoolSize;

    @Value("${folib.threadPool.asyncSecurityPolicy.maxPoolSize}")
    private Integer asyncSecurityPolicyMaxPoolSize;

    @Value("${folib.threadPool.asyncSecurityPolicy.queueCapacity}")
    private Integer asyncSecurityPolicyQueueCapacity;

    @Value("${folib.threadPool.asyncSecurityPolicy.keepAliveSeconds}")
    private Integer asyncSecurityPolicyKeepAliveSeconds;

    @Value("${folib.threadPool.asyncSecurityPolicy.threadNamePrefix}")
    private String asyncSecurityPolicyThreadNamePrefix;

    @Value("${folib.threadPool.asyncSecurityPolicy.awaitTerminationSeconds}")
    private Integer asyncSecurityPolicyAwaitTerminationSeconds;

    @Value("${folib.threadPool.asyncRepositoryCleanup.corePoolSize}")
    private Integer asyncRepositoryCleanupCorePoolSize;

    @Value("${folib.threadPool.asyncRepositoryCleanup.maxPoolSize}")
    private Integer asyncRepositoryCleanupMaxPoolSize;

    @Value("${folib.threadPool.asyncRepositoryCleanup.queueCapacity}")
    private Integer asyncRepositoryCleanupQueueCapacity;

    @Value("${folib.threadPool.asyncRepositoryCleanup.keepAliveSeconds}")
    private Integer asyncRepositoryCleanupKeepAliveSeconds;

    @Value("${folib.threadPool.asyncRepositoryCleanup.threadNamePrefix}")
    private String asyncRepositoryCleanupThreadNamePrefix;

    @Value("${folib.threadPool.asyncRepositoryCleanup.awaitTerminationSeconds}")
    private Integer asyncRepositoryCleanupAwaitTerminationSeconds;

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

    @Value("${folib.threadPool.asyncCronJob.corePoolSize}")
    private Integer asyncCronJobCorePoolSize;

    @Value("${folib.threadPool.asyncCronJob.maxPoolSize}")
    private Integer asyncCronJobMaxPoolSize;

    @Value("${folib.threadPool.asyncCronJob.queueCapacity}")
    private Integer asyncCronJobQueueCapacity;

    @Value("${folib.threadPool.asyncCronJob.keepAliveSeconds}")
    private Integer asyncCronJobKeepAliveSeconds;

    @Value("${folib.threadPool.asyncCronJob.threadNamePrefix}")
    private String asyncCronJobThreadNamePrefix;

    @Value("${folib.threadPool.asyncCronJob.awaitTerminationSeconds}")
    private Integer asyncCronJobAwaitTerminationSeconds;

    @Value("${folib.threadPool.asyncClusterDispatch.corePoolSize}")
    private Integer asyncClusterDispatchCorePoolSize;

    @Value("${folib.threadPool.asyncClusterDispatch.maxPoolSize}")
    private Integer asyncClusterDispatchMaxPoolSize;

    @Value("${folib.threadPool.asyncClusterDispatch.queueCapacity}")
    private Integer asyncClusterDispatchQueueCapacity;

    @Value("${folib.threadPool.asyncClusterDispatch.keepAliveSeconds}")
    private Integer asyncClusterDispatchKeepAliveSeconds;

    @Value("${folib.threadPool.asyncClusterDispatch.threadNamePrefix}")
    private String asyncClusterDispatchThreadNamePrefix;

    @Value("${folib.threadPool.asyncClusterDispatch.awaitTerminationSeconds}")
    private Integer asyncClusterDispatchAwaitTerminationSeconds;

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

    @Value("${folib.threadPool.asyncDownloadArtifact.corePoolSize}")
    private Integer asyncDownloadArtifactCorePoolSize;

    @Value("${folib.threadPool.asyncDownloadArtifact.maxPoolSize}")
    private Integer asyncDownloadArtifactMaxPoolSize;

    @Value("${folib.threadPool.asyncDownloadArtifact.queueCapacity}")
    private Integer asyncDownloadArtifactQueueCapacity;

    @Value("${folib.threadPool.asyncDownloadArtifact.keepAliveSeconds}")
    private Integer asyncDownloadArtifactKeepAliveSeconds;

    @Value("${folib.threadPool.asyncDownloadArtifact.threadNamePrefix}")
    private String asyncDownloadArtifactThreadNamePrefix;

    @Value("${folib.threadPool.asyncDownloadArtifact.awaitTerminationSeconds}")
    private Integer asyncDownloadArtifactAwaitTerminationSeconds;

    @Bean
    public ThreadPoolTaskExecutor asyncThreadPoolTaskExecutor() {
        return buildThreadPoolTaskExecutor(asyncCorePoolSize, asyncMaxPoolSize, asyncQueueCapacity, asyncKeepAliveSeconds, asyncThreadNamePrefix, asyncAwaitTerminationSeconds);
    }

    @Bean
    public ThreadPoolTaskExecutor asyncEventListenerExecutor() {
        return buildThreadPoolTaskExecutor(asyncEventListenerCorePoolSize, asyncEventListenerMaxPoolSize, asyncEventListenerQueueCapacity, asyncEventListenerKeepAliveSeconds, asyncEventListenerThreadNamePrefix, asyncEventListenerAwaitTerminationSeconds);
    }

    @Bean
    public ThreadPoolTaskExecutor asyncStorageThreadPoolExecutor() {
        return buildThreadPoolTaskExecutor(asyncStorageCorePoolSize, asyncStorageMaxPoolSize, asyncStorageQueueCapacity, asyncStorageKeepAliveSeconds, asyncStorageThreadNamePrefix, asyncStorageAwaitTerminationSeconds);
    }

    @Bean
    public ThreadPoolTaskExecutor asyncCronJobThreadPoolExecutor() {
        return buildThreadPoolTaskExecutor(asyncCronJobCorePoolSize, asyncCronJobMaxPoolSize, asyncCronJobQueueCapacity, asyncCronJobKeepAliveSeconds, asyncCronJobThreadNamePrefix, asyncCronJobAwaitTerminationSeconds);
    }

    @Bean
    public ThreadPoolTaskExecutor asyncRepositoryThreadPoolExecutor() {
        return buildThreadPoolTaskExecutor(asyncRepositoryCorePoolSize, asyncRepositoryMaxPoolSize, asyncRepositoryQueueCapacity, asyncRepositoryKeepAliveSeconds, asyncRepositoryThreadNamePrefix, asyncRepositoryAwaitTerminationSeconds);
    }

    @Bean
    public ThreadPoolTaskExecutor asyncClusterDispatchThreadPoolExecutor() {
        return buildThreadPoolTaskExecutor(asyncClusterDispatchCorePoolSize, asyncClusterDispatchMaxPoolSize, asyncClusterDispatchQueueCapacity, asyncClusterDispatchKeepAliveSeconds, asyncClusterDispatchThreadNamePrefix, asyncClusterDispatchAwaitTerminationSeconds);
    }

    @Bean
    public ThreadPoolTaskExecutor asyncSecurityPolicyConfigurationThreadPoolExecutor() {
        return buildThreadPoolTaskExecutor(asyncSecurityPolicyCorePoolSize, asyncSecurityPolicyMaxPoolSize, asyncSecurityPolicyQueueCapacity, asyncSecurityPolicyKeepAliveSeconds, asyncSecurityPolicyThreadNamePrefix, asyncSecurityPolicyAwaitTerminationSeconds);
    }

    @Bean
    public ThreadPoolTaskExecutor asyncConfigThreadPoolExecutor() {
        return buildThreadPoolTaskExecutor(asyncConfigCorePoolSize, asyncConfigMaxPoolSize, asyncConfigQueueCapacity, asyncConfigKeepAliveSeconds, asyncConfigThreadNamePrefix, asyncConfigAwaitTerminationSeconds);
    }

    @Bean
    public ThreadPoolTaskExecutor asyncRepositoryCleanupThreadPoolExecutor() {
        return buildThreadPoolTaskExecutor(
                asyncRepositoryCleanupCorePoolSize,
                asyncRepositoryCleanupMaxPoolSize,
                asyncRepositoryCleanupQueueCapacity,
                asyncRepositoryCleanupKeepAliveSeconds,
                asyncRepositoryCleanupThreadNamePrefix,
                asyncRepositoryCleanupAwaitTerminationSeconds);

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
                asyncScanAwaitTerminationSeconds);
    }

    @Bean
    public ThreadPoolTaskExecutor asyncDownloadArtifactThreadPoolTaskExecutor() {
        return buildThreadPoolTaskExecutor(
                asyncDownloadArtifactCorePoolSize,
                asyncDownloadArtifactMaxPoolSize,
                asyncDownloadArtifactQueueCapacity,
                asyncDownloadArtifactKeepAliveSeconds,
                asyncDownloadArtifactThreadNamePrefix,
                asyncDownloadArtifactAwaitTerminationSeconds);
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
        ThreadPoolTaskExecutor executor = new ThreadPoolTaskExecutor();
        executor.setCorePoolSize(corePoolSize);
        executor.setMaxPoolSize(maxPoolSize);
        executor.setQueueCapacity(queueCapacity);
        executor.setKeepAliveSeconds(keepAliveSeconds);
        executor.setThreadNamePrefix(threadNamePrefix);
        executor.setWaitForTasksToCompleteOnShutdown(true);
        executor.setAwaitTerminationSeconds(awaitTerminationSeconds);
        executor.setRejectedExecutionHandler(new ThreadPoolExecutor.CallerRunsPolicy());
        executor.initialize();
        return executor;
    }
}

