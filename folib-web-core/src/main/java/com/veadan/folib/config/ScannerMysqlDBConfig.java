//package com.veadan.folib.config;
//
//import com.veadan.folib.scanner.config.ScanConfig;
//import com.zaxxer.hikari.HikariDataSource;
//import org.apache.ibatis.session.SqlSessionFactory;
//import org.mybatis.spring.SqlSessionFactoryBean;
//import org.springframework.beans.factory.annotation.Autowired;
//import org.springframework.beans.factory.annotation.Value;
//import org.springframework.boot.context.properties.ConfigurationProperties;
//import org.springframework.context.annotation.Bean;
//import org.springframework.context.annotation.Configuration;
//import org.springframework.core.io.support.PathMatchingResourcePatternResolver;
//import org.springframework.jdbc.datasource.DataSourceTransactionManager;
//import org.springframework.transaction.PlatformTransactionManager;
//
//import javax.sql.DataSource;
//
///**
// * @author xiaodong.wang
// * @email wangxiaodong@beyondcent.com
// * @date 2022/7/15 10:13
// */
//@Configuration
//public class ScannerMysqlDBConfig
//{
//
//	private final static String DATASOURCE_PREIFX = "spring.datasource";
//
//	@Autowired
//	private ScanConfig scanConfig;
//
//	@Bean
//	@ConfigurationProperties(prefix = DATASOURCE_PREIFX)
//	public DataSource dataSource() {
//
//		HikariDataSource dataSource = new HikariDataSource();
//		dataSource.setUsername(scanConfig.getDbUser());
//		dataSource.setPassword(scanConfig.getDbPass());//获取的密码为toor,实际为root，需反转
//		dataSource.setJdbcUrl(scanConfig.getDbUrl());
//		dataSource.setDriverClassName("com.mysql.cj.jdbc.Driver");
//		//最大连接数
//		dataSource.setMaximumPoolSize(50);
//		//最小连接数
//		dataSource.setMinimumIdle(5);
//		return dataSource;
//	}
//
//
//
//	@Bean
//	public SqlSessionFactory sqlSessionFactoryBean() throws Exception {
//
//		SqlSessionFactoryBean sqlSessionFactoryBean = new SqlSessionFactoryBean();
//		sqlSessionFactoryBean.setDataSource(dataSource());
//
//		PathMatchingResourcePatternResolver resolver = new PathMatchingResourcePatternResolver();
//
//		sqlSessionFactoryBean.setMapperLocations(resolver.getResources("classpath:/mapper/*.xml"));
//
//		return sqlSessionFactoryBean.getObject();
//	}
//
//	@Bean
//	public PlatformTransactionManager transactionManager() {
//		return new DataSourceTransactionManager(dataSource());
//	}
//}
