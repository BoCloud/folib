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
package com.folib.users.domain;

import org.springframework.security.core.GrantedAuthority;

import java.util.*;
import java.util.stream.Collectors;

/**
 * Security system atomic item that is used for access restriction. Privileges represent a single permission, such as:
 * Read, Deploy, Admin, View Log etc.
 *
 * @author
 */
public enum Privileges
        implements GrantedAuthority {
    ADMIN,
    ANONYMOUS_USER,
    AUTHENTICATED_USER,
    GLOBAL_CONFIGURATION_MANAGE,
    CONFIGURATION_UPLOAD,
    CONFIGURATION_VIEW,
    CONFIGURATION_SET_INSTANCE_NAME,
    CONFIGURATION_VIEW_INSTANCE_NAME,
    CONFIGURATION_SET_BASE_URL,
    CONFIGURATION_VIEW_BASE_URL,
    CONFIGURATION_SET_PORT,
    CONFIGURATION_VIEW_PORT,
    CONFIGURATION_SET_GLOBAL_PROXY_CFG,
    CONFIGURATION_VIEW_GLOBAL_PROXY_CFG,
    CONFIGURATION_ADD_UPDATE_STORAGE,
    CONFIGURATION_VIEW_STORAGE_CONFIGURATION,
    CONFIGURATION_DELETE_STORAGE_CONFIGURATION,
    CONFIGURATION_ADD_UPDATE_REPOSITORY,
    CONFIGURATION_VIEW_REPOSITORY,
    CONFIGURATION_DELETE_REPOSITORY,
    CONFIGURATION_ADD_LOGGER,
    CONFIGURATION_UPDATE_LOGGER,
    CONFIGURATION_DELETE_LOGGER,
    CONFIGURATION_RETRIEVE_LOG,
    CONFIGURATION_RETRIEVE_LOGBACK_CFG,
    CONFIGURATION_UPLOAD_LOGBACK_CFG,
    MANAGEMENT_REBUILD_METADATA,
    MANAGEMENT_DELETE_METADATA,
    MANAGEMENT_REBUILD_INDEXES,
    ADMIN_CREATE_REPO,
    ADMIN_UPDATE_REPO,
    ADMIN_DELETE_REPO,
    ADMIN_LIST_REPO,
    CREATE_USER,
    UPDATE_USER,
    VIEW_USER,
    IMPERSONATE_USER,
    DELETE_USER,
    ARTIFACTS_DEPLOY,
    ARTIFACTS_DELETE,
    ARTIFACTS_VIEW,
    ARTIFACTS_RESOLVE,
    ARTIFACTS_COPY,
    ARTIFACTS_MOVE,
    ARTIFACTS_PROMOTION,
    ARTIFACTS_MANAGE,
    ARTIFACTS_METADATA,
    SEARCH_ARTIFACTS,
    MANAGEMENT_DELETE_ALL_TRASHES,
    MANAGEMENT_DELETE_TRASH,
    MANAGEMENT_UNDELETE_ALL_TRASHES,
    MANAGEMENT_UNDELETE_TRASH,
    VIEW_OWN_TOKEN,
    VIEW_ANY_TOKEN,
    VIEW_LOGS,
    CONFIGURE_LOGS,
    RSS_FEED,
    UI_LOGIN,
    UI_BROWSE,
    CONFIGURATION_ADD_UPDATE_SECURITY_POLICY,
    CONFIGURATION_VIEW_SECURITY_POLICY_CONFIGURATION,
    CONFIGURATION_DELETE_SECURITY_POLICY_CONFIGURATION,
    CONFIGURATION_ADD_UPDATE_METADATA,
    CONFIGURATION_VIEW_METADATA_CONFIGURATION,
    CONFIGURATION_DELETE_METADATA_CONFIGURATION,
    COMPONENTS_VIEW,
    LICENSES_VIEW,
    VULNERABILITIES_DATABASE_VIEW,
    EXTERNAL_NODE_VIEW,
    EXTERNAL_NODE_SAVE,
    EXTERNAL_NODE_UPDATE,
    EXTERNAL_NODE_DELETE,
    DELETE_USER_GROUP,
    CREATE_USER_GROUP,
    VIEW_USER_GROUP,
    UPDATE_USER_GROUP,
    DELETE_ROLE,
    CREATE_ROLE,
    VIEW_ROLE,
    UPDATE_ROLE
    ;

    /**
     * Helper method for accessing all roles.
     *
     * @return all roles related to full (complete) possible privileges
     */
    public static EnumSet<Privileges> all() {
        EnumSet<Privileges> privileges = EnumSet.allOf(Privileges.class)
                .stream()
                .filter(Privileges::excludeInternalAuthorities)
                .collect(Collectors.toCollection(
                        () -> EnumSet.noneOf(Privileges.class)));
        return privileges;
    }

    public static EnumSet<Privileges> storageAll() {
        return EnumSet.of(
                ARTIFACTS_PROMOTION, CONFIGURATION_ADD_LOGGER, ARTIFACTS_COPY, CONFIGURATION_ADD_UPDATE_STORAGE, CONFIGURATION_SET_GLOBAL_PROXY_CFG,
                ADMIN_CREATE_REPO, CONFIGURATION_SET_INSTANCE_NAME, CONFIGURATION_VIEW_INSTANCE_NAME, CONFIGURATION_DELETE_STORAGE_CONFIGURATION,
                ADMIN_LIST_REPO, CONFIGURATION_DELETE_REPOSITORY, CONFIGURATION_RETRIEVE_LOG, CONFIGURATION_UPLOAD, SEARCH_ARTIFACTS,
                CONFIGURATION_VIEW_BASE_URL, CONFIGURATION_VIEW_REPOSITORY, ARTIFACTS_RESOLVE, CONFIGURATION_UPLOAD_LOGBACK_CFG,
                CONFIGURATION_ADD_UPDATE_REPOSITORY, CONFIGURATION_ADD_UPDATE_METADATA, ARTIFACTS_VIEW, CONFIGURATION_ADD_UPDATE_SECURITY_POLICY,
                CONFIGURATION_SET_BASE_URL, CONFIGURATION_VIEW_METADATA_CONFIGURATION, CONFIGURATION_DELETE_LOGGER, CONFIGURATION_VIEW_STORAGE_CONFIGURATION,
                ARTIFACTS_MOVE, CONFIGURATION_DELETE_SECURITY_POLICY_CONFIGURATION, CONFIGURATION_RETRIEVE_LOGBACK_CFG,
                CONFIGURATION_DELETE_METADATA_CONFIGURATION, CONFIGURATION_SET_PORT, ARTIFACTS_DELETE, ADMIN_UPDATE_REPO, CONFIGURATION_VIEW_PORT,
                ARTIFACTS_DEPLOY, CONFIGURATION_VIEW, VIEW_USER, ADMIN_DELETE_REPO, CONFIGURATION_UPDATE_LOGGER,
                CONFIGURATION_VIEW_SECURITY_POLICY_CONFIGURATION, CONFIGURATION_VIEW_GLOBAL_PROXY_CFG);
    }
    public static EnumSet<Privileges> repoAll() {
        return EnumSet.of(ADMIN_CREATE_REPO, ADMIN_DELETE_REPO, ADMIN_LIST_REPO, ADMIN_UPDATE_REPO);
    }

    public static EnumSet<Privileges> artifactsAll() {
        return EnumSet.of(ARTIFACTS_DEPLOY, ARTIFACTS_DELETE, ARTIFACTS_VIEW, ARTIFACTS_RESOLVE, ARTIFACTS_COPY);
    }

    public static EnumSet<Privileges> usersAll() {
        return EnumSet.of(CREATE_USER, UPDATE_USER, VIEW_USER, IMPERSONATE_USER, DELETE_USER);
    }

    public static EnumSet<Privileges> tokenAll() {
        return EnumSet.of(VIEW_OWN_TOKEN, VIEW_ANY_TOKEN);
    }

    public static EnumSet<Privileges> logsAll() {
        return EnumSet.of(VIEW_LOGS, CONFIGURE_LOGS, RSS_FEED);
    }

    public static EnumSet<Privileges> uiAll() {
        return EnumSet.of(UI_LOGIN, UI_BROWSE);
    }

    public static EnumSet<Privileges> configurationAll() {
        EnumSet<Privileges> privileges = EnumSet.allOf(Privileges.class)
                .stream()
                .filter(Privileges::excludeNonConfigurationAuthorities)
                .collect(Collectors.toCollection(() -> EnumSet.noneOf(Privileges.class)));

        return privileges;
    }

    public static Set<String> anonymous() {
        Set<String> set = new HashSet<>();
        set.add(ARTIFACTS_RESOLVE.name());
        set.add(SEARCH_ARTIFACTS.name());
        return set;
    }

    public static Set<String> r() {
        Set<String> set = new HashSet<>();
        set.add(ARTIFACTS_VIEW.name());
        set.add(ARTIFACTS_RESOLVE.name());

        return set;
    }

    public static Set<String> w() {
        Set<String> set = new HashSet<>();
        set.add(ARTIFACTS_DEPLOY.name());
        set.add(ARTIFACTS_DELETE.name());
        set.add(ARTIFACTS_COPY.name());
        return set;
    }

    public static Set<String> rw() {
        Set<String> set = new HashSet<>();
        set.addAll(r());
        set.addAll(w());

        return set;
    }

    public static Set<Privileges> restricted() {
        Set<Privileges> set = new HashSet<>();
        set.add(ARTIFACTS_RESOLVE);
        set.add(ARTIFACTS_DEPLOY);
        set.add(ARTIFACTS_DELETE);
        set.add(ARTIFACTS_COPY);
        set.add(ARTIFACTS_MOVE);
        set.add(ARTIFACTS_PROMOTION);
        set.add(ARTIFACTS_MANAGE);
        set.add(ARTIFACTS_METADATA);
        return set;
    }

    @Override
    public String getAuthority() {
        return this.name();
    }

    private static boolean excludeInternalAuthorities(Privileges p) {
        List<Privileges> exclude = new ArrayList<>();
        exclude.add(Privileges.ANONYMOUS_USER);
        exclude.add(Privileges.AUTHENTICATED_USER);

        return exclude.stream().noneMatch(p::equals);
    }

    private static boolean excludeNonConfigurationAuthorities(Privileges p) {
        return !p.getAuthority().toLowerCase().matches("^CONFIGURATION_.*") &&
                !p.getAuthority().toLowerCase().equals("GLOBAL_CONFIGURATION_MANAGE");
    }
}
