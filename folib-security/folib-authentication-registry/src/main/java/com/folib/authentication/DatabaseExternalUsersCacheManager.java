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


import com.folib.data.CacheName;
import com.folib.domain.User;
import com.folib.domain.UserEntity;
import com.folib.users.domain.UserData;
import com.folib.users.service.UserAlreadyExistsException;
import com.folib.users.service.impl.RelationalDatabaseUserService;
import com.folib.users.userdetails.FolibExternalUsersCacheManager;
import com.folib.users.userdetails.FolibUserDetails;
import com.folib.util.LocalDateTimeInstance;
import jakarta.transaction.Transactional;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.janusgraph.core.SchemaViolationException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.cache.annotation.CacheEvict;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.stereotype.Component;

import java.util.Optional;

/**
 * @author veadan
 */
@Component
@Transactional
public class DatabaseExternalUsersCacheManager extends RelationalDatabaseUserService implements FolibExternalUsersCacheManager {

    private static final Logger logger = LoggerFactory.getLogger(DatabaseExternalUsersCacheManager.class);

    @Override
    public UserEntity findByUsername(String username) {
        UserEntity result = super.findByUsername(username);
        if (result != null) {
            logger.debug("User found in DB: username=[{}], sourceId=[{}], id=[{}], uuid=[{}]",
                    result.getUsername(),
                    result.getSourceId(),
                    result.getNativeId(),
                    result.getUuid());
        } else {
            logger.info("User not found in DB: username=[{}]", username);
        }

        return result;
    }

    @Override
    @CacheEvict(cacheNames = CacheName.User.AUTHENTICATIONS, key = "#p1.username")
    public User cacheExternalUserDetails(String sourceId,
                                         UserDetails springUser) {
        User user;
        if (springUser instanceof FolibUserDetails) {
            user = ((FolibUserDetails) springUser).getUser();
        } else {
            user = new UserData(springUser);
        }
        String username = user.getUsername();
        Optional<UserEntity> oldUser = Optional.ofNullable(findByUsername(user.getUsername()));
        try {
            // If found user was from another source then remove before save
            Optional<String> oldSource = oldUser.map(User::getSourceId);
            if (oldSource.map(sourceId::equals).filter(Boolean.FALSE::equals).isPresent()) {
//                logger.info("Invalidate user from another source: username=[{}], oldSource=[{}], newSource=[{}]",
//                        username,
//                        oldSource.get(),
//                        sourceId);
//                deleteByUsername(oldUser.map(u -> user.getUuid()).get());
//                oldUser = Optional.empty();
                logger.info("user was from another source");
                throw new RuntimeException("invalid.credentials");
            }

            UserEntity userEntry = oldUser.orElseGet(() -> new UserEntity(username));
            String ldapUserDetailsServiceSourceId = "ldapUserDetailsService";
            if (!ldapUserDetailsServiceSourceId.equalsIgnoreCase(sourceId) && !StringUtils.isBlank(user.getPassword())) {
                userEntry.setPassword(user.getPassword());
            }
            userEntry.setUsername(username);
            userEntry.setEmail(user.getEmail());
            userEntry.setEnabled(user.isEnabled());
            if (CollectionUtils.isEmpty(userEntry.getRoles())) {
                userEntry.setRoles(user.getRoles());
            }
            userEntry.setSecurityTokenKey(user.getSecurityTokenKey());
            userEntry.setLastUpdated(LocalDateTimeInstance.now());
            userEntry.setSourceId(sourceId);

            save(userEntry);
            logger.debug("Cache external user: username=[{}], id=[{}], uuid=[{}], sourceId=[{}], lastUpdated=[{}], UserDetails=[{}]",
                    userEntry.getUsername(),
                    userEntry.getNativeId(),
                    userEntry.getUuid(),
                    userEntry.getSourceId(),
                    userEntry.getLastUpdated(),
                    springUser.getClass().getSimpleName());
            return userEntry;
        } catch (SchemaViolationException e) {
            throw new UserAlreadyExistsException(String.format("Failed to cache external user from [%s], duplicate [%s] already exists.", sourceId,
                    user.getUsername()),
                    e);
        }
    }


}
