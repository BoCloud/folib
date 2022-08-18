//package com.veadan.folib.config;
//
//
//import org.springframework.boot.context.properties.ConfigurationProperties;
//import org.springframework.context.annotation.Bean;
//import org.springframework.context.annotation.Configuration;
//import org.springframework.context.annotation.PropertySource;
//import tk.mybatis.spring.annotation.MapperScan;
//
//@Configuration
//@ConfigurationProperties(prefix = "mysql.core")
//@PropertySource("classpath:mysql-core-jdbc.properties")
//@MapperScan(basePackages = "com.model.mappers",sqlSessionFactoryRef = "mysqlCoreSessionFactory")
//public class MysqlCoreConfig {
//
//    String jdbcUrl;
//    String jdbcUserName;
//    String jdbcPassword;
//    String jdbcDriver;
//    String rootMapper;//mapper文件在classpath下存放的根路径，此处在开发环境中设置的是mappers
//    String aliasesPackage;//别名包
//
//    /**
//     * 设置一个数据库的连接池
//     */
//    @Bean("mysqlCoreDataSource")
//    public DataSource mysqlCoreDataSource(){
//        HikariDataSource dataSource = new HikariDataSource();
//        dataSource.setUsername(this.getJdbcUserName());
//        dataSource.setPassword(this.getRealPassword());//获取的密码为toor,实际为root，需反转
//        dataSource.setJdbcUrl(this.getJdbcUrl());
//        dataSource.setDriverClassName(this.getJdbcDriver());
//        //最大连接数
//        dataSource.setMaximumPoolSize(50);
//        //最小连接数
//        dataSource.setMinimumIdle(5);
//        return dataSource;
//    }
//
//    /**
//     * 密码反转操作
//     */
//    public String getRealPassword(){
//        String jdbcPassword = this.getJdbcPassword();//123456
//        String reverse = StringUtils.reverse(jdbcPassword);//654321
//        return  reverse;
//    }
//
//    /**
//     * 整合mybatis  SqlSessionFactoryBean
//     */
//    @Bean("mysqlCoreSessionFactory")
//    public SqlSessionFactoryBean mysqlCoreSessionFactory(@Qualifier("mysqlCoreDataSource") DataSource dataSource) throws IOException {
//        SqlSessionFactoryBean factoryBean = new SqlSessionFactoryBean();
//        //数据源
//        factoryBean.setDataSource(dataSource);
//        //别名
//        factoryBean.setTypeAliasesPackage(this.getAliasesPackage());
//        //mapper文件存储的位置
//        PathMatchingResourcePatternResolver resolver = new PathMatchingResourcePatternResolver();
//        Resource[] resources = resolver.getResources(this.getMapperFilePath());
//        factoryBean.setMapperLocations(resources);
//        //开启驼峰标识  user_name  --  》 userName
//        org.apache.ibatis.session.Configuration configuration = new org.apache.ibatis.session.Configuration();
//        configuration.setMapUnderscoreToCamelCase(true);
//        factoryBean.setConfiguration(configuration);
//        return factoryBean;
//    }
//    /**
//     * 拼接mapper.xml文件的存储位置
//     */
//    public String getMapperFilePath(){
//        return new StringBuffer("classpath:").append(this.getRootMapper()).append("/**/*.xml").toString();
//    }
//}