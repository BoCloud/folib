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
package com.folib.authentication.registry;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.StringReader;
import java.io.StringWriter;
import java.util.Comparator;
import java.util.HashMap;
import java.util.Map;
import java.util.Optional;
import java.util.TreeMap;
import java.util.function.Predicate;

import javax.inject.Inject;

import com.folib.util.CollectionUtils;
import com.folib.authentication.registry.support.ExternalAuthenticatorsHelper;
import com.folib.authentication.support.AuthenticationConfigurationContext;
import com.folib.authentication.support.AuthenticationContextInitializer;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.ApplicationContext;
import org.springframework.core.env.MapPropertySource;
import org.springframework.core.io.Resource;
import org.springframework.security.authentication.AuthenticationProvider;
import org.springframework.security.core.userdetails.UserDetailsService;
import org.springframework.stereotype.Component;

import com.fasterxml.jackson.dataformat.yaml.YAMLMapper;

/**
 * @author veadan
 * @author veadan
 */
@Component
public class AuthenticationProvidersRegistry
{

    public static final String FOLIB_AUTHENTICATION_PROPERTIES_PREFIX = "folib.authentication";

    private static final Logger logger = LoggerFactory.getLogger(AuthenticationProvidersRegistry.class);

    private YAMLMapper yamlMapper = new YAMLMapper();

    private AuthenticationConfigurationContext authenticationContext;

    private Map<String, Object> authenticationPropertiesMap;

    @Inject
    private ApplicationContext folibApplicationContext;

    @Inject
    private AuthenticationResourceManager authenticationResourceManager;

    @Inject
    private ExternalAuthenticatorsHelper externalAuthenticatorsHelper;


    public void reload()
        throws IOException
    {
        Map<String, Object> authenticationPropertiesMapLocal = fetchAuthenticationProperties();

        reload(authenticationPropertiesMapLocal);
    }

    public void reload(Map<String, Object> authenticationPropertiesMapLocal)
        throws IOException
    {
        logger.info("Reloading authentication configuration...");
        AuthenticationConfigurationContext authenticationContextLocal = createAuthenticationContext(authenticationPropertiesMapLocal);

        apply(authenticationPropertiesMapLocal, authenticationContextLocal);
    }

    private void apply(Map<String, Object> authenticationPropertiesMapLocal,
                       AuthenticationConfigurationContext authenticationContextLocal)
        throws IOException
    {
        logger.info("Applying authentication configuration...");

        if (authenticationContext != null)
        {
            authenticationContext.close();
        }

        Resource resource = authenticationResourceManager.getAuthenticationPropertiesResource();

        StringWriter writer = new StringWriter();
        yamlMapper.writeValue(writer, authenticationPropertiesMapLocal);

        authenticationResourceManager.storeAuthenticationConfigurationResource(resource, new ByteArrayInputStream(
                writer.toString().getBytes()));

        this.authenticationPropertiesMap = authenticationPropertiesMapLocal;
        this.authenticationContext = authenticationContextLocal;
    }

    private Map<String, Object> fetchAuthenticationProperties()
        throws IOException
    {
        Resource authenticationPropertiesResource = authenticationResourceManager.getAuthenticationPropertiesResource();
        Map<String, Object> authenticationPropertiesMapLocal = yamlMapper.readValue(authenticationPropertiesResource.getInputStream(),Map.class);
//        logger.info("===============》"+authenticationPropertiesMapLocal.toString());
        return authenticationPropertiesMapLocal;
    }

    private AuthenticationConfigurationContext createAuthenticationContext(Map<String, Object> authenticationPropertiesMap)
        throws IOException
    {
        AuthenticationConfigurationContext authenticationContext = new AuthenticationConfigurationContext();

        ClassLoader entryClassLoader = folibApplicationContext.getClassLoader();
        ClassLoader requiredClassLoader = externalAuthenticatorsHelper.getExternalAuthenticatorsClassLoader(entryClassLoader);

        Resource authenticationConfigurationResource = authenticationResourceManager.getAuthenticationConfigurationResource();
        authenticationContext.setParent(folibApplicationContext);
        authenticationContext.setClassLoader(requiredClassLoader);
        authenticationContext.load(authenticationConfigurationResource);

        AuthenticationContextInitializer contextInitializer = new AuthenticationContextInitializer(
                new MapPropertySource(AuthenticationContextInitializer.FOLIB_AUTHENTICATION_PROVIDERS,
                        CollectionUtils.flattenMap(authenticationPropertiesMap)));
        contextInitializer.initialize(authenticationContext);

        authenticationContext.refresh();

        return authenticationContext;
    }

    public Map<String, UserDetailsService> getUserDetailsServiceMap()
    {
        Map<String, UserDetailsService> componentMap = authenticationContext.getBeansOfType(UserDetailsService.class);

        TreeMap<String, UserDetailsService> result = new TreeMap<>(new AutnenticationComponentOrderComparator());

        result.putAll(componentMap);

        return result;
    }

    public Map<String, AuthenticationProvider> getAuthenticationProviderMap()
    {
        Map<String, AuthenticationProvider> componentMap = authenticationContext.getBeansOfType(AuthenticationProvider.class);

        TreeMap<String, AuthenticationProvider> result = new TreeMap<>(new AutnenticationComponentOrderComparator());
        result.putAll(componentMap);

        return result;
    }

    public Map<String, Object> getAuthenticationProperties()
    {
        Object resutl = CollectionUtils.getMapValue(FOLIB_AUTHENTICATION_PROPERTIES_PREFIX,
                                                    authenticationPropertiesMap);

        return new HashMap<>((Map<String, Object>) resutl);
    }

    public Map<String, Object> getAuthenticationProperties(String path)
    {
        Object resutl = CollectionUtils.getMapValue(FOLIB_AUTHENTICATION_PROPERTIES_PREFIX + "." + path,
                                                    authenticationPropertiesMap);

        return new HashMap<>((Map<String, Object>) resutl);
    }

    private class AutnenticationComponentOrderComparator implements Comparator<String>
    {

        @Override
        public int compare(String id1,
                           String id2)
        {
            Map<String, Object> authenticationMap = Optional.of(authenticationPropertiesMap.get("folib"))
                                                            .map(o -> (Map<String, Object>) o)
                                                            .map(o -> (Map<String, Object>) o.get("authentication"))
                                                            .get();

            Map<String, Object> componentConfiguration1 = (Map<String, Object>) authenticationMap.get(id1);
            Map<String, Object> componentConfiguration2 = (Map<String, Object>) authenticationMap.get(id2);

            Integer order1 = Optional.ofNullable(componentConfiguration1).map(c -> (Integer) c.get("order")).orElse(0);
            Integer order2 = Optional.ofNullable(componentConfiguration2).map(c -> (Integer) c.get("order")).orElse(0);

            return order1.compareTo(order2);
        }

    }

    public MergePropertiesContext mergeProperties()
        throws IOException
    {
        return new MergePropertiesContext(authenticationPropertiesMap);
    }

    public class MergePropertiesContext
    {

        private Map<String, Object> target;

        public MergePropertiesContext(Map<String, Object> target)
            throws IOException
        {
            super();

            StringWriter w = new StringWriter();

            yamlMapper.writeValue(w, target);
            this.target = yamlMapper.readValue(new StringReader(w.toString()), Map.class);
        }

        public MergePropertiesContext merge(String path,
                                            Map<String, Object> map)
        {
            CollectionUtils.putMapValue(FOLIB_AUTHENTICATION_PROPERTIES_PREFIX + "." + path, map, target);

            return this;
        }

        public boolean apply(Predicate<ApplicationContext> p)
            throws IOException
        {
            AuthenticationConfigurationContext authenticationContextLocal = createAuthenticationContext(target);
            if (!p.test(authenticationContextLocal))
            {
                return false;
            }

            AuthenticationProvidersRegistry.this.apply(target, authenticationContextLocal);

            return true;
        }

        public boolean apply()
            throws IOException
        {
            return apply(c -> true);
        }

    }
}
