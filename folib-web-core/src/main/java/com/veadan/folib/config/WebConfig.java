package com.veadan.folib.config;

import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.veadan.folib.configuration.ConfigurationManager;
import com.veadan.folib.configuration.FolibSecurityConfig;
import com.veadan.folib.constant.GlobalConstants;
import com.veadan.folib.converters.RoleFormToRoleConverter;
import com.veadan.folib.converters.RoleListFormToRoleListConverter;
import com.veadan.folib.converters.configuration.*;
import com.veadan.folib.converters.cron.CronTaskConfigurationFormToCronTaskConfigurationDtoConverter;
import com.veadan.folib.converters.storage.routing.RoutingRuleFormToMutableConverter;
import com.veadan.folib.converters.users.AccessModelFormToUserAccessModelDtoConverter;
import com.veadan.folib.converters.users.UserFormToUserDtoConverter;
import com.veadan.folib.cron.config.CronTasksConfig;
import com.veadan.folib.interceptors.PermissionCheckInterceptor;
import com.veadan.folib.jtwig.extensions.ByteSizeConversionExtension;
import com.veadan.folib.mapper.WebObjectMapperSubtypes;
import com.veadan.folib.services.DirectoryListingService;
import com.veadan.folib.services.DirectoryListingServiceImpl;
import com.veadan.folib.utils.CustomAntPathMatcher;
import com.veadan.folib.web.CustomRequestMappingHandlerMapping;
import com.veadan.folib.web.DirectoryTraversalFilter;
import com.veadan.folib.web.RepositoryMethodArgumentResolver;
import com.veadan.folib.yaml.YAMLMapperFactory;
import jakarta.inject.Inject;
import jakarta.inject.Named;
import jakarta.servlet.http.HttpServletRequest;
import org.apache.commons.lang.StringUtils;
import org.jtwig.environment.EnvironmentConfigurationBuilder;
import org.jtwig.spring.boot.config.JtwigViewResolverConfigurer;
import org.jtwig.web.servlet.JtwigRenderer;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.autoconfigure.jdbc.DataSourceAutoConfiguration;
import org.springframework.boot.autoconfigure.web.servlet.WebMvcRegistrations;
import org.springframework.boot.web.servlet.ServletComponentScan;
import org.springframework.cache.annotation.EnableCaching;
import org.springframework.context.annotation.*;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Import;
import org.springframework.core.io.Resource;
import org.springframework.format.FormatterRegistry;
import org.springframework.http.MediaType;
import org.springframework.http.converter.*;
import org.springframework.http.converter.json.MappingJackson2HttpMessageConverter;
import org.springframework.validation.Validator;
import org.springframework.validation.beanvalidation.LocalValidatorFactoryBean;
import org.springframework.web.context.request.RequestContextListener;
import org.springframework.web.filter.CommonsRequestLoggingFilter;
import org.springframework.web.filter.RequestContextFilter;
import org.springframework.web.method.support.HandlerMethodArgumentResolver;
import org.springframework.web.multipart.MultipartResolver;
import org.springframework.web.servlet.config.annotation.*;
import org.springframework.web.servlet.mvc.method.annotation.RequestMappingHandlerMapping;
import org.springframework.web.servlet.resource.PathResourceResolver;
import org.springframework.web.servlet.resource.ResourceResolverChain;
import org.springframework.web.servlet.view.InternalResourceView;
import org.springframework.web.servlet.view.InternalResourceViewResolver;


import java.nio.charset.StandardCharsets;
import java.util.Arrays;
import java.util.List;

@Configuration
@ComponentScan({"com.veadan.folib.controllers",
        "com.veadan.folib.controllers",
        "com.veadan.folib.validation",
        "com.veadan.folib.web",
        "com.veadan.folib.mapper",
        "com.veadan.folib.utils",
        "com.veadan.folib.scanner",
        "com.veadan.folib.gremlin",
        "com.veadan.folib.actuator",
        "com.veadan.folib.components",
        "com.veadan.folib.listener",
        "com.veadan.folib.eventlistener",
        "com.veadan.folib.promotion",
        "com.veadan.folib.task",
        "com.veadan.folib.aop",
        "com.veadan.folib.ws",
        "com.veadan.folib.filter"})
@Import({CommonConfig.class,
        FolibSecurityConfig.class,
        StorageApiConfig.class,
        EventsConfig.class,
        StorageCoreConfig.class,
        UsersConfig.class,
        WebSecurityConfig.class,
        ClientConfig.class,
        CronTasksConfig.class,
        DataSourceAutoConfiguration.class,
        SwaggerConfig.class})
@EnableCaching(order = 105)
@ServletComponentScan(basePackages = {"com.veadan.folib.filter"})
public class WebConfig
        implements WebMvcConfigurer ,WebMvcRegistrations
{

    private static final Logger logger = LoggerFactory.getLogger(WebConfig.class);

    @Inject
    @Named("customAntPathMatcher")
    private CustomAntPathMatcher antPathMatcher;

    @Inject
    private ObjectMapper objectMapper;

    @Inject
    private YAMLMapperFactory yamlMapperFactory;

    @Inject
    private ConfigurationManager configurationManager;

    @Value("${folib.custom.maxInMemorySize:10240}")
    private int maxInMemorySize;

    WebConfig() {
        logger.info("Initialized web configuration.");
    }

    @Bean
    RequestContextListener requestContextListener() {
        return new RequestContextListener();
    }

    @Bean
    RequestContextFilter requestContextFilter() {
        return new RequestContextFilter();
    }

    @Bean
    CommonsRequestLoggingFilter commonsRequestLoggingFilter() {
        CommonsRequestLoggingFilter result = new CommonsRequestLoggingFilter() {

            @Override
            protected String createMessage(HttpServletRequest request,
                                           String prefix,
                                           String suffix) {
                return super.createMessage(request, String.format("%smethod=%s;", prefix, request.getMethod()), suffix);
            }

        };
        result.setIncludeQueryString(true);
        result.setIncludeHeaders(true);
        result.setIncludeClientInfo(true);
        return result;
    }

    @Override
    public RequestMappingHandlerMapping getRequestMappingHandlerMapping() {
       return new CustomRequestMappingHandlerMapping();
    }

    //@Override
    //protected RequestMappingHandlerMapping createRequestMappingHandlerMapping() {
    //    return new CustomRequestMappingHandlerMapping();
    //}

    @Bean
    DirectoryTraversalFilter directoryTraversalFilter() {
        return new DirectoryTraversalFilter();
    }

    @Override
    public void configureMessageConverters(List<HttpMessageConverter<?>> converters) {
        StringHttpMessageConverter stringConverter = new StringHttpMessageConverter();
        stringConverter.setWriteAcceptCharset(false);
        stringConverter.setDefaultCharset(StandardCharsets.UTF_8);
        // if your argument is a byte[]
        converters.add(new ByteArrayHttpMessageConverter());
        converters.add(stringConverter);
        converters.add(new FormHttpMessageConverter());

        //converters.add(yamlConverter());
        converters.add(jackson2Converter());
        converters.add(new ResourceHttpMessageConverter());
    }


    @Override
    public void configureContentNegotiation(ContentNegotiationConfigurer configurer) {
        configurer.favorPathExtension(false);
    }

    // TODO consider using the same MappingJackson2HttpMessageConverter for yaml and json !
    //@Bean
    //public MappingJackson2HttpMessageConverter yamlConverter() {
    //    MappingJackson2HttpMessageConverter yamlConverter = new MappingJackson2HttpMessageConverter(
    //            yamlMapperFactory.create(WebObjectMapperSubtypes.INSTANCE.subtypes()));
    //    yamlConverter.setSupportedMediaTypes(
    //            Arrays.asList(MediaType.valueOf(com.veadan.folib.net.MediaType.APPLICATION_YML_VALUE), MediaType.valueOf(com.veadan.folib.net.MediaType.APPLICATION_YAML_VALUE)));
    //    return yamlConverter;
    //}

    @Bean
    public MappingJackson2HttpMessageConverter jackson2Converter() {
        objectMapper.configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false);
        MappingJackson2HttpMessageConverter mappingJackson2HttpMessageConverter = new MappingJackson2HttpMessageConverter(objectMapper);
        mappingJackson2HttpMessageConverter.setDefaultCharset(StandardCharsets.UTF_8);
        return mappingJackson2HttpMessageConverter;
    }

    @Bean
    public Validator localValidatorFactoryBean() {
        return new LocalValidatorFactoryBean();
    }

    @Bean
    @Qualifier("loggingManagementDirectoryListingService")
    public DirectoryListingService getLoggingManagementDirectoryListingService() {
        return createDirectoryListingServiceForTemplate("%s/api/logging");
    }

    private DirectoryListingService createDirectoryListingServiceForTemplate(String template) {
        String baseUrl = StringUtils.chomp(configurationManager.getConfiguration().getBaseUrl(), "/");
        String finalUrl = String.format(template, baseUrl);
        return new DirectoryListingServiceImpl(finalUrl);
    }


    @Bean
    @Qualifier("browseRepositoryDirectoryListingService")
    public DirectoryListingService getBrowseRepositoryDirectoryListingService() {
        return createDirectoryListingServiceForTemplate("%s/api/browse");
    }

    @Override
    public void configurePathMatch(PathMatchConfigurer configurer) {
        configurer.setUseRegisteredSuffixPatternMatch(false)
                .setUseSuffixPatternMatch(false)
                .setUseTrailingSlashMatch(true)
                .setPathMatcher(antPathMatcher);
    }



    @Override
    public void addResourceHandlers(ResourceHandlerRegistry registry) {
        //registry.setOrder(-1);

        registry.addResourceHandler("/rest/**")
                .addResourceLocations("classpath:/META-INF/resources/docs/rest/")
                .setCachePeriod(3600);

        registry.addResourceHandler("*.html")
                .addResourceLocations("classpath:/")
                .setCachePeriod(3600);

        String webUrlPrefix = System.getProperty(GlobalConstants.WEB_URL_PREFIX);
        if (StringUtils.isBlank(webUrlPrefix)) {
            webUrlPrefix = "/ui/";
        }
        registry.addResourceHandler(webUrlPrefix + "**")
                .addResourceLocations("classpath:/ui/")
                .setCachePeriod(3600)
                .resourceChain(true)
                //.addResolver(new GzipResourceResolver())
                .addResolver(new PathResourceResolver());
        registry.addResourceHandler("/help","/help/","/help/**")
                .addResourceLocations("classpath:/docs/")
                .setCachePeriod(3600)
                .resourceChain(true)
                //.addResolver(new GzipResourceResolver())
                .addResolver(new HtmlFallbackResourceResolver()); // 替换为自定义解析器

        registry.addResourceHandler("/webjars/**")
                .addResourceLocations("classpath:/META-INF/resources/webjars/");

    }

    @Override
    public void addFormatters(FormatterRegistry registry) {
        registry.addConverter(new RoleFormToRoleConverter());
        registry.addConverter(new RoleListFormToRoleListConverter());
        registry.addConverter(UserFormToUserDtoConverter.INSTANCE);
        registry.addConverter(AccessModelFormToUserAccessModelDtoConverter.INSTANCE);
        registry.addConverter(ProxyConfigurationFormConverter.INSTANCE);
        registry.addConverter(new RoutingRuleFormToMutableConverter());
        registry.addConverter(StorageFormConverter.INSTANCE);
        registry.addConverter(RepositoryFormConverter.INSTANCE);
        registry.addConverter(RemoteRepositoryFormConverter.INSTANCE);
        registry.addConverter(CronTaskConfigurationFormToCronTaskConfigurationDtoConverter.INSTANCE);
        registry.addConverter(RepositoryPermissionFormConverter.INSTANCE);
    }

    @Bean
    JtwigViewResolverConfigurer jtwigViewResolverConfigurer() {
        return jtwigViewResolver -> {
            JtwigRenderer renderer = new JtwigRenderer(
                    EnvironmentConfigurationBuilder.configuration()
                            .extensions()
                            .add(new ByteSizeConversionExtension())
                            .and()
                            .build());
            jtwigViewResolver.setRenderer(renderer);
            jtwigViewResolver.setPrefix("classpath:/views/");
            jtwigViewResolver.setSuffix(".twig.html");
            jtwigViewResolver.setViewNames("directoryListing");
            jtwigViewResolver.setOrder(0);
        };
    }

    @Bean
    InternalResourceViewResolver internalResourceViewResolver() {
        InternalResourceViewResolver viewResolver = new InternalResourceViewResolver();
        viewResolver.setViewClass(InternalResourceView.class);
        viewResolver.setViewNames("*.html");
        viewResolver.setOrder(1);
        viewResolver.setRedirectHttp10Compatible(false);
        return viewResolver;
    }

//    @Bean
//    MavenArtifactRequestInterceptor mavenArtifactRequestInterceptor(RepositoryPathResolver repositoryPathResolver) {
//        return new MavenArtifactRequestInterceptor(repositoryPathResolver);
//    }

    @Bean
    PermissionCheckInterceptor permissionCheckInterceptor() {
        return new PermissionCheckInterceptor();
    }

    @Override
    public void addArgumentResolvers(List<HandlerMethodArgumentResolver> argumentResolvers) {
        argumentResolvers.add(repositoryMethodArgumentResolver());
    }

    @Bean
    public RepositoryMethodArgumentResolver repositoryMethodArgumentResolver() {
        return new RepositoryMethodArgumentResolver();
    }

    //@Bean(name = "multipartResolver")
    //public MultipartResolver multipartResolver() {
    //    CustomMultipartResolver resolver = new CustomMultipartResolver();
    //    resolver.setMaxInMemorySize(maxInMemorySize);
    //    return resolver;
    ////}

    @Override
    public void addViewControllers(ViewControllerRegistry registry) {
        registry.addRedirectViewController("/", "/ui/index.html");
    }

    public class HtmlFallbackResourceResolver extends PathResourceResolver {
        @Override
        protected Resource resolveResourceInternal(HttpServletRequest request, String requestPath, List<? extends Resource> locations, ResourceResolverChain chain) {
            Resource resource = super.resolveResourceInternal(request, requestPath, locations, chain);
            if (resource == null) {
                // 尝试添加 .html 后缀
                return super.resolveResourceInternal(request, "index.html", locations, chain);
            }
            return resource;
        }
    }

}
