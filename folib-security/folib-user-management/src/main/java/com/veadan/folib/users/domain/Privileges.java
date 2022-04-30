package com.veadan.folib.users.domain;

import java.util.*;
import java.util.stream.Collectors;

import org.springframework.security.core.GrantedAuthority;

/**
 * Security system atomic item that is used for access restriction. Privileges represent a single permission, such as:
 * Read, Deploy, Admin, View Log etc.
 *
 * @author Alex Oreshkevich
 * @see {@linkplain https://dev.carlspring.org/youtrack/issue/SB-122}
 * @see {@linkplain https://dev.carlspring.org/youtrack/issue/SB-126}
 */
public enum Privileges
        implements GrantedAuthority
{
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
    UI_BROWSE;

    /**
     * Helper method for accessing all roles.
     *
     * @return all roles related to full (complete) possible privileges
     */
    public static EnumSet<Privileges> all()
    {
        EnumSet<Privileges> privileges = EnumSet.allOf(Privileges.class)
                                                .stream()
                                                .filter(Privileges::excludeInternalAuthorities)
                                                .collect(Collectors.toCollection(
                                                        () -> EnumSet.noneOf(Privileges.class)));
        return privileges;
    }

    public static EnumSet<Privileges> repoAll()
    {
        return EnumSet.of(ADMIN_CREATE_REPO, ADMIN_DELETE_REPO, ADMIN_LIST_REPO, ADMIN_UPDATE_REPO);
    }

    public static EnumSet<Privileges> artifactsAll()
    {
        return EnumSet.of(ARTIFACTS_DEPLOY, ARTIFACTS_DELETE, ARTIFACTS_VIEW, ARTIFACTS_RESOLVE, ARTIFACTS_COPY);
    }

    public static EnumSet<Privileges> usersAll()
    {
        return EnumSet.of(CREATE_USER, UPDATE_USER, VIEW_USER, IMPERSONATE_USER, DELETE_USER);
    }

    public static EnumSet<Privileges> tokenAll()
    {
        return EnumSet.of(VIEW_OWN_TOKEN, VIEW_ANY_TOKEN);
    }

    public static EnumSet<Privileges> logsAll()
    {
        return EnumSet.of(VIEW_LOGS, CONFIGURE_LOGS, RSS_FEED);
    }

    public static EnumSet<Privileges> uiAll()
    {
        return EnumSet.of(UI_LOGIN, UI_BROWSE);
    }

    public static EnumSet<Privileges> configurationAll()
    {
        EnumSet<Privileges> privileges = EnumSet.allOf(Privileges.class)
                                                .stream()
                                                .filter(Privileges::excludeNonConfigurationAuthorities)
                                                .collect(Collectors.toCollection(() -> EnumSet.noneOf(Privileges.class)));

        return privileges;
    }

    public static Set<String> r()
    {
        Set<String> set = new HashSet<>();
        set.add(ARTIFACTS_VIEW.name());
        set.add(ARTIFACTS_RESOLVE.name());

        return set;
    }

    public static Set<String> w()
    {
        Set<String> set = new HashSet<>();
        set.add(ARTIFACTS_DEPLOY.name());
        set.add(ARTIFACTS_DELETE.name());
        set.add(ARTIFACTS_COPY.name());

        return set;
    }

    public static Set<String> rw()
    {
        Set<String> set = new HashSet<>();
        set.addAll(r());
        set.addAll(w());

        return set;
    }

    @Override
    public String getAuthority()
    {
        return this.name();
    }

    private static boolean excludeInternalAuthorities(Privileges p)
    {
        List<Privileges> exclude = new ArrayList<>();
        exclude.add(Privileges.ANONYMOUS_USER);
        exclude.add(Privileges.AUTHENTICATED_USER);

        return exclude.stream().noneMatch(p::equals);
    }

    private static boolean excludeNonConfigurationAuthorities(Privileges p)
    {
        return !p.getAuthority().toLowerCase().matches("^CONFIGURATION_.*") &&
               !p.getAuthority().toLowerCase().equals("GLOBAL_CONFIGURATION_MANAGE");
    }
}
