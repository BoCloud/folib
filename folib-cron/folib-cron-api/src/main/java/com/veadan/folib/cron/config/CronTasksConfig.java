package com.veadan.folib.cron.config;

import java.util.concurrent.*;

import javax.inject.Inject;

import cn.hutool.core.thread.ThreadFactoryBuilder;
import com.veadan.folib.config.DataServiceConfig;
import com.veadan.folib.cron.services.impl.CronTaskExecutor;
import com.veadan.folib.config.StorageCoreConfig;
import lombok.extern.slf4j.Slf4j;
import org.springframework.context.ApplicationContext;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Import;
import org.springframework.scheduling.concurrent.ThreadPoolTaskExecutor;
import org.springframework.scheduling.quartz.SchedulerFactoryBean;
import org.springframework.scheduling.quartz.SpringBeanJobFactory;

@Configuration
@ComponentScan({ "com.veadan.folib.cron",
                 "com.veadan.folib.event.cron",
                 "com.veadan.folib.dependency.snippet" })
@Import({ DataServiceConfig.class,
          StorageCoreConfig.class
})
@Slf4j
public class CronTasksConfig
{

    @Inject
    private ApplicationContext applicationContext;

    @Bean
    public SchedulerFactoryBean schedulerFactoryBean()
    {
        SchedulerFactoryBean schedulerFactoryBean = new SchedulerFactoryBean();
        schedulerFactoryBean.setJobFactory(springBeanJobFactory());
        schedulerFactoryBean.setTaskExecutor(cronJobTaskExecutor());
        
        return schedulerFactoryBean;
    }
    
    @Bean
    public Executor cronJobTaskExecutor()
    {
        int poolSize = Runtime.getRuntime().availableProcessors() * 2;
        return new CronTaskExecutor(poolSize, poolSize, 10, TimeUnit.SECONDS, new LinkedBlockingQueue<>(), ThreadFactoryBuilder.create().setNamePrefix("cron-task-pool-").build());
    }

    @Bean
    public SpringBeanJobFactory springBeanJobFactory()
    {
        AutowiringSpringBeanJobFactory jobFactory = new AutowiringSpringBeanJobFactory();
        jobFactory.setApplicationContext(applicationContext);

        return jobFactory;
    }

    private ThreadPoolTaskExecutor buildThreadPoolTaskExecutor(Integer corePoolSize, Integer maxPoolSize, Integer keepAliveSeconds) {
        ThreadPoolTaskExecutor executor = new ThreadPoolTaskExecutor();
        executor.setCorePoolSize(corePoolSize);
        executor.setMaxPoolSize(maxPoolSize);
        Integer queueCapacity = 1000000;
        executor.setQueueCapacity(queueCapacity);
        executor.setKeepAliveSeconds(keepAliveSeconds);
        executor.setThreadNamePrefix("cron-task-pool-");
        executor.setWaitForTasksToCompleteOnShutdown(true);
        executor.setAwaitTerminationSeconds(10);
        executor.setRejectedExecutionHandler(new ThreadPoolExecutor.CallerRunsPolicy());
        executor.initialize();
        log.info("Cron thread pool name [{}] core size [{}] max size [{}] queue capacity [{}]", executor.getThreadNamePrefix(), executor.getCorePoolSize(), executor.getMaxPoolSize(), queueCapacity);
        return executor;
    }

}
