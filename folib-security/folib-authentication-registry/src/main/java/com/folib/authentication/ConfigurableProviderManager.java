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
package com.folib.authentication;

import com.alibaba.fastjson.JSONObject;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.datatype.jsr310.JavaTimeModule;
import com.folib.authentication.api.AuthenticationItem;
import com.folib.authentication.api.AuthenticationItemConfigurationManager;
import com.folib.authentication.api.AuthenticationItems;
import com.folib.authentication.api.CustomAuthenticationItemMapper;
import com.folib.authentication.registry.AuthenticationProvidersRegistry;
import com.folib.authentication.support.AuthenticationConfigurationContext;
import com.folib.components.DistributedCacheComponent;
import com.folib.configuration.ConfigurationManager;
import com.folib.domain.SecurityRoleEntity;
import com.folib.domain.User;
import com.folib.domain.UserEntity;
import com.folib.entity.FolibUser;
import com.folib.mapper.FolibUserMapper;
import com.folib.users.service.UserAlreadyExistsException;
import com.folib.users.userdetails.FolibExternalUsersCacheManager;
import com.folib.users.userdetails.UserDetailsMapper;
import com.folib.util.LocalDateTimeInstance;
import jakarta.transaction.Transactional;
import org.apache.commons.lang3.StringUtils;
import org.apache.logging.log4j.util.Strings;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.ApplicationContext;
import org.springframework.context.annotation.Primary;
import org.springframework.context.event.ContextRefreshedEvent;
import org.springframework.context.event.EventListener;
import org.springframework.security.authentication.AuthenticationManager;
import org.springframework.security.authentication.AuthenticationProvider;
import org.springframework.security.authentication.InternalAuthenticationServiceException;
import org.springframework.security.authentication.ProviderManager;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.AuthenticationException;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.security.core.userdetails.UserDetailsService;
import org.springframework.security.core.userdetails.UsernameNotFoundException;
import org.springframework.stereotype.Component;
import org.springframework.web.context.request.RequestContextHolder;
import org.springframework.web.context.request.ServletRequestAttributes;

import javax.inject.Inject;
import jakarta.servlet.http.HttpServletRequest;
import java.io.IOException;
import java.time.LocalDateTime;
import java.util.*;
import java.util.concurrent.TimeUnit;
import java.util.function.Predicate;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/**
 * @author veadan
 */
@Primary
@Component
public class ConfigurableProviderManager extends ProviderManager implements UserDetailsService, AuthenticationItemConfigurationManager {

    private static final Logger logger = LoggerFactory.getLogger(ConfigurableProviderManager.class);

    @Inject
    private AuthenticationProvidersRegistry authenticationProvidersRegistry;

    @Inject
    private UserDetailsMapper userDetailsMapper;

    @Inject
    private FolibExternalUsersCacheManager folibExternalUsersCacheManager;

    @Inject
    protected ConfigurationManager configurationManager;

    @Inject
    private FolibUserMapper folibUserMapper;
    @Inject
    private DistributedCacheComponent distributedCacheComponent;

    private final Map<String, AuthenticationProvider> authenticationProviderMap = new HashMap<>();

    private final Map<String, UserDetailsService> userProviderMap = new LinkedHashMap<>();

    public ConfigurableProviderManager() {
        super(new ArrayList<>(), new EmptyAuthenticationManager());
    }

    public void reload()
            throws IOException {
        logger.info("Reloading Authentication.");

        authenticationProvidersRegistry.reload();

        reloadAuthenticationItems();
    }

    private void reloadAuthenticationItems() {
        authenticationProviderMap.clear();
        authenticationProviderMap.putAll(authenticationProvidersRegistry.getAuthenticationProviderMap());

        userProviderMap.clear();
        userProviderMap.putAll(authenticationProvidersRegistry.getUserDetailsServiceMap());
    }

    @Override
    public List<AuthenticationProvider> getProviders() {
        return new ArrayList<>(authenticationProviderMap.values());
    }

    @Override
    @Transactional
    public UserDetails loadUserByUsername(String username)
            throws UsernameNotFoundException {
        return loadUserDetails(username).map(userDetailsMapper)
                .orElseThrow(() -> new UsernameNotFoundException(username));
    }

    private Optional<User> loadUserDetails(String username) {
        String userKey = String.format("user_%s", username);
        String userInfo = distributedCacheComponent.get(userKey);

        ObjectMapper objectMapper = new ObjectMapper();
        objectMapper.registerModule(new JavaTimeModule());
        if (Strings.isNotEmpty(userInfo)) {
            UserEntity userEntity = getUserEntity(userInfo);
            return Optional.of(userEntity);
            /*User folibUser;
            try {
                folibUser = objectMapper.readValue(userInfo, UserEntity.class);
            } catch (JsonProcessingException e) {
                throw new RuntimeException(e);
            }
            Optional<User> optionalUser = Optional.ofNullable(folibUser).filter(this::isInternalOrValidExternalUser);
            if (optionalUser.isPresent()) {
                return optionalUser;
            }*/
        }else {
            Optional<User> optionalUser = Optional.ofNullable(folibExternalUsersCacheManager.findByUsername(username)).filter(this::isInternalOrValidExternalUser);
            if (optionalUser.isPresent()) {
                try {
                    distributedCacheComponent.put(userKey, objectMapper.writeValueAsString(optionalUser.get()), 30, TimeUnit.MINUTES);
                } catch (JsonProcessingException e) {
                    throw new RuntimeException(e);
                }
                return optionalUser;
            }
        }

        return loadExternalUserDetails(username);
    }

    private static  UserEntity getUserEntity(String userInfo) {
        UserEntity userEntity = JSONObject.parseObject(userInfo, UserEntity.class);
        JSONObject jsonObject = JSONObject.parseObject(userInfo);
        Object roles = jsonObject.get("roles");
        List<SecurityRoleEntity> securityRoleEntityList = JSONObject.parseArray(roles.toString(), SecurityRoleEntity.class);
        //JSONObject lastUpdated = JSONObject.parseObject(jsonObject.get("lastUpdated").toString());
        //String datetimeString = lastUpdated.get("year") + "-" + String.format("%02d",lastUpdated.get("monthValue")) + "-" + String.format("%02d",lastUpdated.get("dayOfMonth")) + " " + String.format("%02d",lastUpdated.get("hour")) + ":" + String.format("%02d",lastUpdated.get("minute")) + ":" + String.format("%02d",lastUpdated.get("second"));
        //DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss");
        //LocalDateTime localDateTime = LocalDateTime.parse(datetimeString, formatter);
        //userEntity.setLastUpdated(localDateTime);
        userEntity.setRoles(new HashSet<>(securityRoleEntityList));
        return userEntity;
    }

    protected Optional<User> loadExternalUserDetails(String username) {
        String userKey = String.format("user_%s", username);
        ObjectMapper objectMapper = new ObjectMapper();
        objectMapper.registerModule(new JavaTimeModule());
        FolibUser folibUser = folibUserMapper.selectOne(Wrappers.<FolibUser>lambdaQuery().eq(FolibUser::getUsername, username));
        if (Objects.nonNull(folibUser) && StringUtils.isNotBlank(folibUser.getSourceId())) {
            String sourceId = folibUser.getSourceId();
            UserDetailsService userDetailsService = userProviderMap.get(sourceId);
            UserDetails externalUser = null;
            try {
                externalUser = userDetailsService.loadUserByUsername(username);
            } catch (UsernameNotFoundException e) {
                logger.info("User [{}] not found from [{}]", username, sourceId);
            }
            try {
                User user = folibExternalUsersCacheManager.cacheExternalUserDetails(sourceId, externalUser);
                try {
                    distributedCacheComponent.put(userKey, objectMapper.writeValueAsString(user), 30, TimeUnit.MINUTES);
                } catch (JsonProcessingException e) {
                    throw new RuntimeException(e);
                }
                return Optional.of(user);
            } catch (UserAlreadyExistsException e) {
                logger.info(String.format("Retry to load user [%s] from [%s] by reason [%s]",
                        username, sourceId, e.getMessage()));
                return loadUserDetails(username);
            }
        } else {
            HttpServletRequest httpServletRequest = ((ServletRequestAttributes) (RequestContextHolder.currentRequestAttributes())).getRequest();
//            String loginType = httpServletRequest.getHeader("X-Folibrary-Login-Type");
//            String ldapUserDetailsServiceSourceId = "ldapUserDetailsService";
            for (Map.Entry<String, UserDetailsService> userDetailsServiceEntry : userProviderMap.entrySet()) {
                String sourceId = userDetailsServiceEntry.getKey();
//                if (ldapUserDetailsServiceSourceId.equals(sourceId) && !LoginTypeEnum.LDAP.getType().equalsIgnoreCase(loginType)) {
//                    continue;
//                }
                UserDetailsService userDetailsService = userDetailsServiceEntry.getValue();

                UserDetails externalUser;
                try {
                    externalUser = userDetailsService.loadUserByUsername(username);
                } catch (Exception e) {
                    logger.info("User [{}] not found from [{}]", username, sourceId);
                    continue;
                }
                try {
                    User user = folibExternalUsersCacheManager.cacheExternalUserDetails(sourceId, externalUser);
                    try {
                        distributedCacheComponent.put(userKey, objectMapper.writeValueAsString(user), 30, TimeUnit.MINUTES);
                    } catch (JsonProcessingException e) {
                        throw new RuntimeException(e);
                    }
                    return Optional.of(user);

                } catch (UserAlreadyExistsException e) {
                    logger.info(String.format("Retry to load user [%s] from [%s] by reason [%s]",
                            username, sourceId, e.getMessage()));

                    return loadUserDetails(username);
                }
            }
        }
        return Optional.empty();
    }

    private boolean isInternalOrValidExternalUser(User user) {
        Integer timeout = configurationManager.getSessionTimeoutSeconds();

        LocalDateTime userLastUpdate = Optional.ofNullable(user.getLastUpdated())
                .orElse(LocalDateTime.MIN);
        LocalDateTime userExpireDate = userLastUpdate.plusSeconds(timeout);
        LocalDateTime nowDate = LocalDateTimeInstance.now();
        return StringUtils.isBlank(user.getSourceId()) || nowDate.isBefore(userExpireDate);
    }

    @Override
    public Authentication authenticate(Authentication authentication)
            throws AuthenticationException {
        try {
            return super.authenticate(authentication);
        } catch (InternalAuthenticationServiceException e) {
            logger.error(String.format("Failed to authenticate user [%s]", authentication.getName()), e);
            throw e;
        }
    }

    public void reorder(String first,
                        String second)
            throws IOException {
        Map<String, Object> p1 = authenticationProvidersRegistry.getAuthenticationProperties(first);
        Map<String, Object> p2 = authenticationProvidersRegistry.getAuthenticationProperties(second);

        assertValidConfiguration(first, p1);
        assertValidConfiguration(second, p2);

        Integer orderFirst = (Integer) p1.get("order");
        Integer orderSecond = (Integer) p2.get("order");

        p1.put("order", orderSecond);
        p2.put("order", orderFirst);

        if (!authenticationProvidersRegistry.mergeProperties()
                .merge(first, p1)
                .merge(second, p2)
                .apply()) {
            return;
        }

        reloadAuthenticationItems();
    }

    @Override
    public void updateAuthenticationItems(AuthenticationItems items)
            throws IOException {

        AuthenticationProvidersRegistry.MergePropertiesContext mergePropertiesContext = authenticationProvidersRegistry.mergeProperties();
        for (AuthenticationItem item : items.getAuthenticationItemList()) {
            Map<String, Object> properties = authenticationProvidersRegistry.getAuthenticationProperties(item.getName());

            properties.put("order", item.getOrder());
            properties.put("enabled", Boolean.TRUE.equals(item.getEnabled()));

            mergePropertiesContext = mergePropertiesContext.merge(item.getName(), properties);

        }

        boolean result = mergePropertiesContext.apply();
        if (!result) {
            return;
        }

        reloadAuthenticationItems();
    }

    private void assertValidConfiguration(String authenticationItemId,
                                          Map<String, Object> p1) {
        if (p1 == null || p1.get("order") == null) {
            throw new IllegalArgumentException(
                    String.format("Invalid authentication configuration for [%s]", authenticationItemId));
        }
    }

    @Override
    public AuthenticationItems getAuthenticationItems() {
        Map<String, Object> authenticationProperties = authenticationProvidersRegistry.getAuthenticationProperties();

        AuthenticationItems result = new AuthenticationItems();
        result.getAuthenticationItemList()
                .addAll(Stream.concat(authenticationItemStream(AuthenticationProvider.class, authenticationProviderMap,
                        authenticationProperties),
                        authenticationItemStream(UserDetailsService.class, userProviderMap,
                                authenticationProperties))
                        .collect(Collectors.toList()));

        return result;
    }

    @Override
    public <T> T getCustomAuthenticationItem(CustomAuthenticationItemMapper<T> mapper) {
        Map<String, Object> map = authenticationProvidersRegistry.getAuthenticationProperties(mapper.getConfigurationItemId());

        return mapper.map(map);
    }

    @Override
    public <T> void putCustomAuthenticationItem(T customAuthenticationItem,
                                                CustomAuthenticationItemMapper<T> mapper)
            throws IOException {
        String itemId = mapper.getConfigurationItemId();

        authenticationProvidersRegistry.mergeProperties()
                .merge(itemId, mapper.map(customAuthenticationItem))
                .apply();
    }

    @Override
    public <T> void testCustomAuthenticationItem(T customAuthenticationItem,
                                                 CustomAuthenticationItemMapper<T> mapper,
                                                 Predicate<ApplicationContext> p)
            throws IOException {
        String itemId = mapper.getConfigurationItemId();

        authenticationProvidersRegistry.mergeProperties()
                .merge(itemId, mapper.map(customAuthenticationItem))
                .apply(p);
    }

    private <T> Stream<AuthenticationItem> authenticationItemStream(Class<T> itemClass,
                                                                    Map<String, T> sourceMap,
                                                                    Map<String, Object> authenticationProperties) {
        return sourceMap.entrySet()
                .stream()
                .map(e -> new AuthenticationItem(
                        e.getKey(),
                        itemClass.getSimpleName(),
                        (Map<String, Object>) authenticationProperties.get(e.getKey())))
                .sorted((i1,
                         i2) -> i1.getOrder()
                        .compareTo(i2.getOrder()));
    }

    @EventListener({ContextRefreshedEvent.class})
    public void contextRefreshedEvent(ContextRefreshedEvent e)
            throws IOException {
        ApplicationContext applicationContext = e.getApplicationContext();
        if (applicationContext == null || applicationContext instanceof AuthenticationConfigurationContext) {
            return;
        }

        reload();
    }

    public static final class EmptyAuthenticationManager implements AuthenticationManager {

        @Override
        public Authentication authenticate(Authentication authentication)
                throws AuthenticationException {
            return null;
        }

    }

    public static final class AuthenticationNotConfiguredException extends AuthenticationException {

        public AuthenticationNotConfiguredException() {
            super("Authentication should be configured with `folib-authentication-providers.xml`");
        }

    }

}
