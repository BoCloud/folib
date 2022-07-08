package com.veadan.folib.config;

import com.veadan.folib.security.CustomAccessDeniedHandler;
import com.veadan.folib.security.authentication.Http401AuthenticationEntryPoint;
import com.veadan.folib.security.authentication.FolibAuthenticationFilter;
import com.veadan.folib.security.authentication.suppliers.AuthenticationSupplier;
import com.veadan.folib.security.authentication.suppliers.AuthenticationSuppliers;
import com.veadan.folib.security.vote.MethodAccessDecisionManager;
import com.veadan.folib.authentication.AuthenticationConfig;
import com.veadan.folib.services.ConfigurationManagementService;
import com.veadan.folib.users.domain.SystemRole;
import com.veadan.folib.users.security.AuthoritiesProvider;

import javax.inject.Inject;
import javax.inject.Qualifier;

import java.util.ArrayList;
import java.util.List;

import org.apache.commons.lang.BooleanUtils;
import org.springframework.boot.actuate.autoconfigure.security.servlet.EndpointRequest;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Import;
import org.springframework.security.access.AccessDecisionManager;
import org.springframework.security.authentication.AuthenticationManager;
import org.springframework.security.authentication.AuthenticationTrustResolver;
import org.springframework.security.authentication.AuthenticationTrustResolverImpl;
import org.springframework.security.config.annotation.authentication.builders.AuthenticationManagerBuilder;
import org.springframework.security.config.annotation.method.configuration.EnableGlobalMethodSecurity;
import org.springframework.security.config.annotation.method.configuration.GlobalMethodSecurityConfiguration;
import org.springframework.security.config.annotation.web.builders.HttpSecurity;
import org.springframework.security.config.annotation.web.builders.WebSecurity;
import org.springframework.security.config.annotation.web.configuration.EnableWebSecurity;
import org.springframework.security.config.annotation.web.configuration.WebSecurityConfigurerAdapter;
import org.springframework.security.config.http.SessionCreationPolicy;
import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.core.authority.AuthorityUtils;
import org.springframework.security.web.AuthenticationEntryPoint;
import org.springframework.security.web.access.AccessDeniedHandler;
import org.springframework.security.web.access.ExceptionTranslationFilter;
import org.springframework.security.web.authentication.AnonymousAuthenticationFilter;
import org.springframework.security.web.firewall.DefaultHttpFirewall;
import org.springframework.web.cors.CorsConfiguration;
import org.springframework.web.cors.CorsConfigurationSource;
import org.springframework.web.cors.UrlBasedCorsConfigurationSource;

@ComponentScan({ "com.veadan.folib.security" })
@Import({ DataServiceConfig.class,
          UsersConfig.class,
          AuthenticationConfig.class})
@Configuration
@EnableWebSecurity
public class WebSecurityConfig
        extends WebSecurityConfigurerAdapter
{

    @Inject
    private AuthoritiesProvider authoritiesProvider;

    @Inject
    private AuthenticationManager authenticationManager;

    @Inject
    private List<AuthenticationSupplier> suppliers;


    @Override
    public void init(WebSecurity web)
            throws Exception
    {
        super.init(web);
        DefaultHttpFirewall httpFirewall = new DefaultHttpFirewall();
        httpFirewall.setAllowUrlEncodedSlash(true);
        web.httpFirewall(httpFirewall);
    }

    @Override
    protected void configure(HttpSecurity http)
            throws Exception
    {
        http.addFilterAfter(folibAuthenticationFilter(),
                            ExceptionTranslationFilter.class)
            .sessionManagement()
            .sessionCreationPolicy(SessionCreationPolicy.STATELESS)
            .and()
            .exceptionHandling()
            .accessDeniedHandler(accessDeniedHandler())
            // TODO SB-813
            .authenticationEntryPoint(customBasicAuthenticationEntryPoint())
            .and()
            // this part of code is necessary to secure endpoints for not authorized users
            .authorizeRequests()
            .requestMatchers(EndpointRequest.toAnyEndpoint())
            .hasAuthority("ADMIN")
            .and()
            .anonymous()
            .authenticationFilter(anonymousAuthenticationFilter())
            .and()
            .cors()
            .and()
            .csrf()
            .disable();
    }

    @Override
    protected void configure(AuthenticationManagerBuilder auth) throws Exception {

    }

    @Bean
    public CorsConfigurationSource corsConfigurationSource(ConfigurationManagementService configurationManagementService)
    {
        final CorsConfiguration configuration = new CorsConfiguration();
        final com.veadan.folib.configuration.CorsConfiguration internalCorsConfiguration = configurationManagementService
                                                                                                           .getConfiguration()
                                                                                                           .getCorsConfiguration();
        if (internalCorsConfiguration != null)
        {
            if (internalCorsConfiguration.getAllowedMethods() != null)
            {
                configuration.setAllowedMethods(new ArrayList<>(internalCorsConfiguration.getAllowedMethods()));
            }
            if (internalCorsConfiguration.getAllowedHeaders() != null)
            {
                configuration.setAllowedHeaders(new ArrayList<>(internalCorsConfiguration.getAllowedHeaders()));
            }
            if (internalCorsConfiguration.getAllowedOrigins() != null)
            {
                configuration.setAllowedOriginPatterns(new ArrayList<>(internalCorsConfiguration.getAllowedOrigins()));
            }
            if (internalCorsConfiguration.getExposedHeaders() != null)
            {
                configuration.setExposedHeaders(new ArrayList<>(internalCorsConfiguration.getExposedHeaders()));
            }
            if (internalCorsConfiguration.getAllowCredentials() != null)
            {
                configuration.setAllowCredentials(BooleanUtils.isTrue(internalCorsConfiguration.getAllowCredentials()));
            }
            if (internalCorsConfiguration.getMaxAge() != null)
            {
                configuration.setMaxAge(internalCorsConfiguration.getMaxAge());
            }
        }

        final UrlBasedCorsConfigurationSource source = new UrlBasedCorsConfigurationSource();
        source.registerCorsConfiguration("/**", configuration);
        return source;
    }

    @Bean
    AccessDeniedHandler accessDeniedHandler()
    {
        return new CustomAccessDeniedHandler();
    }

    @Bean
    @UnauthorizedEntyPoint
    AuthenticationEntryPoint customBasicAuthenticationEntryPoint()
    {
        return new Http401AuthenticationEntryPoint();
    }

    @Bean
    FolibAuthenticationFilter folibAuthenticationFilter()
    {
        return new FolibAuthenticationFilter(new AuthenticationSuppliers(suppliers), authenticationManager);
    }


    @Bean
    AnonymousAuthenticationFilter anonymousAuthenticationFilter()
    {
        List<GrantedAuthority> authorities = AuthorityUtils.createAuthorityList("ROLE_ANONYMOUS");
        authorities.addAll(authoritiesProvider.getRuntimeRole(SystemRole.ANONYMOUS.name()).getAccessModel().getApiAuthorities());

        return new AnonymousAuthenticationFilter("folib-unique-key",
                                                 "anonymousUser",
                                                 authorities);
    }



    /**
     * This Configuration enables @PreAuthorize annotations
     *
     * @author @author veadan
     */
    @Configuration
    @EnableGlobalMethodSecurity(prePostEnabled = true)
    public static class MethodSecurityConfig
            extends GlobalMethodSecurityConfiguration
    {

        @Inject
        MethodAccessDecisionManager methodAccessDecisionManager;

        @Override
        protected AccessDecisionManager accessDecisionManager()
        {
            return methodAccessDecisionManager;
        }

    }

    @Configuration
    public static class SharedObjectsConfig{

        @Bean
        AuthenticationTrustResolver authenticationTrustResolver() {
            return new AuthenticationTrustResolverImpl();
        }
        
    }

    @Qualifier
    public static @interface UnauthorizedEntyPoint
    {

    }
}
