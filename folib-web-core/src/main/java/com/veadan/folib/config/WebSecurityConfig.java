package com.veadan.folib.config;

import com.veadan.folib.authentication.AuthenticationConfig;
import com.veadan.folib.authorization.dto.Role;
import com.veadan.folib.security.CustomAccessDeniedHandler;
import com.veadan.folib.security.authentication.FolibAuthenticationFilter;
import com.veadan.folib.security.authentication.suppliers.AuthenticationSupplier;
import com.veadan.folib.security.authentication.suppliers.AuthenticationSuppliers;
import com.veadan.folib.security.vote.ExtendedAuthoritiesVoter;
import com.veadan.folib.security.vote.ExtendedAuthorizationManager;

import com.veadan.folib.services.ConfigurationManagementService;
import com.veadan.folib.users.domain.SystemRole;
import com.veadan.folib.users.security.AuthoritiesProvider;
import org.apache.commons.lang.BooleanUtils;
import org.springframework.beans.factory.InitializingBean;
import org.springframework.boot.actuate.autoconfigure.security.servlet.EndpointRequest;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Import;
import org.springframework.http.HttpStatus;
import org.springframework.security.access.AccessDecisionManager;
import org.springframework.security.access.expression.method.DefaultMethodSecurityExpressionHandler;
import org.springframework.security.access.expression.method.MethodSecurityExpressionHandler;
import org.springframework.security.authentication.AuthenticationManager;
import org.springframework.security.authentication.AuthenticationTrustResolver;
import org.springframework.security.authentication.AuthenticationTrustResolverImpl;
import org.springframework.security.authentication.BadCredentialsException;
import org.springframework.security.authorization.method.AuthorizationManagerBeforeMethodInterceptor;
import org.springframework.security.config.annotation.authentication.builders.AuthenticationManagerBuilder;
import org.springframework.security.config.annotation.method.configuration.EnableGlobalMethodSecurity;
import org.springframework.security.config.annotation.method.configuration.EnableMethodSecurity;
import org.springframework.security.config.annotation.method.configuration.GlobalMethodSecurityConfiguration;
import org.springframework.security.config.annotation.web.builders.HttpSecurity;
import org.springframework.security.config.annotation.web.builders.WebSecurity;
import org.springframework.security.config.annotation.web.configuration.EnableWebSecurity;
import org.springframework.security.config.annotation.web.configuration.WebSecurityCustomizer;
import org.springframework.security.config.http.SessionCreationPolicy;
import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.core.authority.AuthorityUtils;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.security.web.AuthenticationEntryPoint;
import org.springframework.security.web.SecurityFilterChain;
import org.springframework.security.web.access.AccessDeniedHandler;
import org.springframework.security.web.access.ExceptionTranslationFilter;
import org.springframework.security.web.authentication.AnonymousAuthenticationFilter;
import org.springframework.security.web.authentication.UsernamePasswordAuthenticationFilter;
import org.springframework.security.web.authentication.www.BasicAuthenticationEntryPoint;
import org.springframework.security.config.annotation.web.configurers.AbstractHttpConfigurer;
import org.springframework.security.web.context.SecurityContextPersistenceFilter;
import org.springframework.security.web.firewall.HttpFirewall;
import org.springframework.security.web.firewall.StrictHttpFirewall;
import org.springframework.web.cors.CorsConfiguration;
import org.springframework.web.cors.CorsConfigurationSource;
import org.springframework.web.cors.UrlBasedCorsConfigurationSource;

import javax.inject.Inject;
import javax.inject.Qualifier;
import java.util.ArrayList;
import java.util.List;

import static org.springframework.security.config.Customizer.withDefaults;


@Configuration
@ComponentScan({"com.veadan.folib.security"})
@Import({DataServiceConfig.class,
        UsersConfig.class,
        AuthenticationConfig.class})
@EnableWebSecurity
@EnableMethodSecurity(prePostEnabled = true)
public class WebSecurityConfig {

    @Inject
    private AuthoritiesProvider authoritiesProvider;

    @Inject
    private AuthenticationManager authenticationManager;

    @Inject
    private List<AuthenticationSupplier> suppliers;

    @Bean
    public SecurityFilterChain securityFilterChain(HttpSecurity http) throws Exception {
        http .addFilterBefore(folibAuthenticationFilter(), UsernamePasswordAuthenticationFilter.class)
                .exceptionHandling(exception -> exception
                        .accessDeniedHandler(accessDeniedHandler())
                        //.authenticationEntryPoint(customBasicAuthenticationEntryPoint())
                )
                .authorizeHttpRequests(auth -> auth
                        .requestMatchers(EndpointRequest.toAnyEndpoint()).hasAuthority("ADMIN")
                        .requestMatchers("/dav/**").authenticated()
                        .requestMatchers("/favicon.ico", "/ui/**", "/docs/**", "/webjars/**", "/rest/**").permitAll()
                        .anyRequest().permitAll())
                .anonymous(anon -> anon
                        .authenticationFilter(anonymousAuthenticationFilter()))
                .cors(withDefaults())
                .csrf(AbstractHttpConfigurer::disable);

        return http.build();
    }

    // 替换 WebSecurity 自定义配置
    @Bean
    public HttpFirewall allowUrlEncodedSlashHttpFirewall() {
        StrictHttpFirewall firewall = new StrictHttpFirewall();
        firewall.setAllowUrlEncodedSlash(true);
        return firewall;
    }

    @Bean
    public CorsConfigurationSource corsConfigurationSource(ConfigurationManagementService configurationManagementService) {
        final CorsConfiguration configuration = new CorsConfiguration();
        final com.veadan.folib.configuration.CorsConfiguration internalCorsConfiguration = configurationManagementService
                .getConfiguration()
                .getCorsConfiguration();
        if (internalCorsConfiguration != null) {
            if (internalCorsConfiguration.getAllowedMethods() != null) {
                configuration.setAllowedMethods(new ArrayList<>(internalCorsConfiguration.getAllowedMethods()));
            }
            if (internalCorsConfiguration.getAllowedHeaders() != null) {
                configuration.setAllowedHeaders(new ArrayList<>(internalCorsConfiguration.getAllowedHeaders()));
            }
            if (internalCorsConfiguration.getAllowedOrigins() != null) {
                configuration.setAllowedOriginPatterns(new ArrayList<>(internalCorsConfiguration.getAllowedOrigins()));
            }
            if (internalCorsConfiguration.getExposedHeaders() != null) {
                configuration.setExposedHeaders(new ArrayList<>(internalCorsConfiguration.getExposedHeaders()));
            }
            if (internalCorsConfiguration.getAllowCredentials() != null) {
                configuration.setAllowCredentials(BooleanUtils.isTrue(internalCorsConfiguration.getAllowCredentials()));
            }
            if (internalCorsConfiguration.getMaxAge() != null) {
                configuration.setMaxAge(internalCorsConfiguration.getMaxAge());
            }
        }

        final UrlBasedCorsConfigurationSource source = new UrlBasedCorsConfigurationSource();
        source.registerCorsConfiguration("/**", configuration);
        return source;
    }

    @Bean
    AccessDeniedHandler accessDeniedHandler() {
        return new CustomAccessDeniedHandler();
    }

    @Bean
    @UnauthorizedEntyPoint
    AuthenticationEntryPoint customBasicAuthenticationEntryPoint() {
        return new PathSpecificBasicAuthenticationEntryPoint();
    }

    @Bean
    FolibAuthenticationFilter folibAuthenticationFilter() {
        return new FolibAuthenticationFilter(new AuthenticationSuppliers(suppliers), authenticationManager,customBasicAuthenticationEntryPoint());
    }


    @Bean
    AnonymousAuthenticationFilter anonymousAuthenticationFilter() {
        List<GrantedAuthority> authorities = AuthorityUtils.createAuthorityList("ROLE_ANONYMOUS");

        Role role = authoritiesProvider.getRuntimeRole(SystemRole.ANONYMOUS.name());
        authorities.addAll(role.getAccessModel().getApiAuthorities());
        return new AnonymousAuthenticationFilter("folib-unique-key",
                "anonymousUser",
                authorities);
    }

    //@Configuration
    //public static class MethodSecurityConfig {
    //
    //    @Inject
    //    MethodAccessDecisionManager methodAccessDecisionManager;
    //
    //    @Bean
    //    public MethodSecurityExpressionHandler methodSecurityExpressionHandler() {
    //        DefaultMethodSecurityExpressionHandler handler = new DefaultMethodSecurityExpressionHandler();
    //        // 根据需要配置表达式处理
    //        return handler;
    //    }
    //
    //    @Bean
    //    public AccessDecisionManager accessDecisionManager() {
    //        return methodAccessDecisionManager;
    //    }
    //}

    @Configuration
    public static class SharedObjectsConfig {

        @Bean
        AuthenticationTrustResolver authenticationTrustResolver() {
            return new AuthenticationTrustResolverImpl();
        }

    }

    @Qualifier
    public static @interface UnauthorizedEntyPoint {

    }

    @Bean
    public InitializingBean initializingBean() {
        return () -> SecurityContextHolder.setStrategyName(SecurityContextHolder.MODE_INHERITABLETHREADLOCAL);
    }

    //  解决静态资源访问403,并对url 进行解码
    @Bean
    public WebSecurityCustomizer webSecurityCustomizer(HttpFirewall firewall) {
        return web -> web.httpFirewall(firewall);
    }

    @Bean
    public AuthorizationManagerBeforeMethodInterceptor extendedInterceptor(ExtendedAuthoritiesVoter extendedAuthoritiesVoter) {
        return AuthorizationManagerBeforeMethodInterceptor
                .preAuthorize(new ExtendedAuthorizationManager(extendedAuthoritiesVoter));
    }
}
