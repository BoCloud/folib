package com.veadan.folib.cron.config;

import java.util.concurrent.Executor;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.TimeUnit;

import javax.inject.Inject;

import com.veadan.folib.config.DataServiceConfig;
import com.veadan.folib.cron.services.impl.CronTaskExecutor;
import com.veadan.folib.config.StorageCoreConfig;
import org.springframework.context.ApplicationContext;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Import;
import org.springframework.scheduling.quartz.SchedulerFactoryBean;
import org.springframework.scheduling.quartz.SpringBeanJobFactory;

@Configuration
@ComponentScan({ "com.veadan.folib.cron",
                 "com.veadan.folib.event.cron",
                 "com.veadan.folib.dependency.snippet" })
@Import({ DataServiceConfig.class,
          StorageCoreConfig.class
})
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
        return new CronTaskExecutor(10, 10, 10, TimeUnit.SECONDS, new LinkedBlockingQueue<>());
    }

    @Bean
    public SpringBeanJobFactory springBeanJobFactory()
    {
        AutowiringSpringBeanJobFactory jobFactory = new AutowiringSpringBeanJobFactory();
        jobFactory.setApplicationContext(applicationContext);

        return jobFactory;
    }

}
