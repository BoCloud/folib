package com.folib.users.dto;

import com.folib.users.domain.Privileges;

import java.util.Set;

public interface StoragePrivileges
{

    Set<? extends RepositoryPrivileges> getRepositoryPrivileges();

    Set<Privileges> getStoragePrivileges();

    String getStorageId();

}
