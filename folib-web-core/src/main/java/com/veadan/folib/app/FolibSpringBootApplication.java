package com.veadan.folib.app;

import com.veadan.folib.config.WebConfig;
import com.veadan.folib.config.janusgraph.FolibCassandraEmbeddedProperties;
import com.veadan.folib.config.janusgraph.JanusGraphDbProfile;
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
import org.springframework.boot.context.properties.ConfigurationPropertiesScan;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
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
@MapperScan(basePackages = {"com.veadan.folib.scanner.mapper", "com.veadan.folib.mapper"})
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
