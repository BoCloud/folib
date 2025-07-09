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
package com.folib.users.userdetails;

import com.folib.domain.User;
import org.springframework.security.core.userdetails.UserDetails;

/**
 * This interface purpose is to manage folib external users cache within
 * authentication components.<br>
 * Extarnal users stored near with regular users but the difference is that
 * external users have not empty value of {@link User#getSourceId()}, which
 * pointing to external system ID where the external user come from.<br>
 * Another thing specific for external users is that they have configurable
 * cache invalidation time, so when it expires then the external user should be
 * invalidated and deleted. So the value of {@link User#getLastUpdated()} updated
 * every time
 * {@link FolibExternalUsersCacheManager#cacheExternalUserDetails(String, UserDetails)}
 * method called, this extends the period during which the external user remains
 * valid.
 * 
 * @author veadan
 */
public interface FolibExternalUsersCacheManager
{

    /**
     * Searches the user within folib users storage. The user can be
     * internal folib user, or external user chached in users storage. <br>
     * 
     * @param username
     *            the username to search
     * 
     * @return the {@link User} instance found or null
     */
    User findByUsername(String username);

    /**
     * Updates or creates external users which is chaced in folib users
     * storage.<br>
     * This method call also updates the {@link User#getLastUpdated()} value.
     * 
     * @param externalSourceId
     *            the identifier of external users provider
     * @param user
     *            the {@link UserDetails} to be used to update the cached
     *            {@link User}
     * @return the cached {@link User} instance
     */
    User cacheExternalUserDetails(String externalSourceId,
                                  UserDetails user);

    /**
     * Deletes stored {@link User} instance from folib users storage. This
     * is commonly needed to invalidate external users cache.
     * 
     * @param username
     *            the username to search the {@link User} for delete
     */
    void deleteByUsername(String username);

}
