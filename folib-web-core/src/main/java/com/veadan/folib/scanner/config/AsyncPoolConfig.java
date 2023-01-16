package com.veadan.folib.scanner.config;

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

    @Value("${folib.threadPool.asyncMetadata.corePoolSize}")
    private Integer asyncMetadataCorePoolSize;

    @Value("${folib.threadPool.asyncMetadata.maxPoolSize}")
    private Integer asyncMetadataMaxPoolSize;

    @Value("${folib.threadPool.asyncMetadata.queueCapacity}")
    private Integer asyncMetadataQueueCapacity;

    @Value("${folib.threadPool.asyncMetadata.keepAliveSeconds}")
    private Integer asyncMetadataKeepAliveSeconds;

    @Value("${folib.threadPool.asyncMetadata.threadNamePrefix}")
    private String asyncMetadataThreadNamePrefix;

    @Value("${folib.threadPool.asyncMetadata.awaitTerminationSeconds}")
    private Integer asyncMetadataAwaitTerminationSeconds;

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
    public ThreadPoolTaskExecutor asyncRepositoryThreadPoolExecutor() {
        return buildThreadPoolTaskExecutor(asyncRepositoryCorePoolSize, asyncRepositoryMaxPoolSize, asyncRepositoryQueueCapacity, asyncRepositoryKeepAliveSeconds, asyncRepositoryThreadNamePrefix, asyncRepositoryAwaitTerminationSeconds);
    }

    @Bean
    public ThreadPoolTaskExecutor asyncSecurityPolicyConfigurationThreadPoolExecutor() {
        return buildThreadPoolTaskExecutor(asyncSecurityPolicyCorePoolSize, asyncSecurityPolicyMaxPoolSize, asyncSecurityPolicyQueueCapacity, asyncSecurityPolicyKeepAliveSeconds, asyncSecurityPolicyThreadNamePrefix, asyncSecurityPolicyAwaitTerminationSeconds);
    }

    @Bean
    public ThreadPoolTaskExecutor asyncMetadataConfigurationThreadPoolExecutor() {
        return buildThreadPoolTaskExecutor(asyncMetadataCorePoolSize, asyncMetadataMaxPoolSize, asyncMetadataQueueCapacity, asyncMetadataKeepAliveSeconds, asyncMetadataThreadNamePrefix, asyncMetadataAwaitTerminationSeconds);
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

