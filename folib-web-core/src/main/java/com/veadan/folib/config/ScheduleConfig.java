package com.veadan.folib.config;

import org.apache.commons.lang3.concurrent.BasicThreadFactory;
import org.springframework.context.annotation.Configuration;
import org.springframework.scheduling.annotation.EnableScheduling;
import org.springframework.scheduling.annotation.SchedulingConfigurer;
import org.springframework.scheduling.config.ScheduledTaskRegistrar;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.ScheduledThreadPoolExecutor;
import java.util.concurrent.ThreadPoolExecutor;

/**
 * @author leipenghui
 * @date 2024/3/11
 **/
@Configuration
@EnableScheduling
public class ScheduleConfig implements SchedulingConfigurer {

    @Override
    public void configureTasks(ScheduledTaskRegistrar taskRegistrar) {
        int corePoolSize = Runtime.getRuntime().availableProcessors();
        ScheduledExecutorService executorService = new ScheduledThreadPoolExecutor(corePoolSize,
                new BasicThreadFactory.
                        Builder().namingPattern("scheduled-pool-%d").daemon(true).build(), new ThreadPoolExecutor.CallerRunsPolicy());
        taskRegistrar.setScheduler(executorService);
    }
}
