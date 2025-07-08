package com.folib.users.domain;

/**
 * Defines the restricted system roles. The purpose is to restrict changing or
 * deleting some of the configured roles.
 * 
 * @author veadan
 */
public enum SystemRole
{
    ADMIN,
    READERS,
    UI_MANAGER,
    REPOSITORY_MANAGER,
    ARTIFACTS_MANAGER,
    ANONYMOUS,
    GENERAL,
    OPEN_SOURCE_MANAGE;

}
