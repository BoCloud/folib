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
package com.folib.app;

import com.folib.config.WebConfig;
import com.folib.config.janusgraph.JanusGraphDbProfile;
import org.mybatis.spring.annotation.MapperScan;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.boot.ApplicationArguments;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.boot.autoconfigure.cassandra.CassandraAutoConfiguration;
import org.springframework.boot.autoconfigure.data.neo4j.Neo4jDataAutoConfiguration;
import org.springframework.boot.autoconfigure.jdbc.DataSourceAutoConfiguration;
import org.springframework.boot.autoconfigure.orm.jpa.HibernateJpaAutoConfiguration;
import org.springframework.boot.context.metrics.buffering.BufferingApplicationStartup;
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.context.annotation.Import;
import org.springframework.scheduling.annotation.EnableAsync;

/**
 * @author Veadan
 */
@SpringBootApplication(exclude = {DataSourceAutoConfiguration.class,
        HibernateJpaAutoConfiguration.class,
        Neo4jDataAutoConfiguration.class,
        CassandraAutoConfiguration.class})
@Import(WebConfig.class)
@MapperScan(basePackages = {"com.folib.scanner.mapper", "com.folib.mapper"})
@EnableAsync
public class FolibSpringBootApplication {

    private static final Logger logger = LoggerFactory.getLogger(FolibSpringBootApplication.class);

    private static ConfigurableApplicationContext applicationContext;

    public static void main(String[] args) {
        if (System.getProperty(JanusGraphDbProfile.PROPERTY_PROFILE) == null) {
            logger.info("JanusGraphDb profile not set, will use [{}] profile as default",
                    JanusGraphDbProfile.PROFILE_EMBEDDED);

            System.setProperty(JanusGraphDbProfile.PROPERTY_PROFILE, JanusGraphDbProfile.PROFILE_EMBEDDED);
        }
        System.setProperty("sendCredentialsOverHttp", "true");
        SpringApplication application = new SpringApplication(FolibSpringBootApplication.class);
        application.setApplicationStartup(new BufferingApplicationStartup(1500));
        applicationContext = application.run(args);
        applicationContext.start();
    }

    public static void restart() {
        ApplicationArguments args = applicationContext.getBean(ApplicationArguments.class);

        Thread thread = new Thread(() -> {
            applicationContext.close();
            applicationContext = SpringApplication.run(FolibSpringBootApplication.class, args.getSourceArgs());
        });

        thread.setDaemon(false);
        thread.start();
    }
}
