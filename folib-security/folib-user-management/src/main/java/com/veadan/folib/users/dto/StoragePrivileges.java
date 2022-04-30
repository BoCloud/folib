package com.veadan.folib.users.dto;

import java.util.Set;

public interface StoragePrivileges
{

    Set<? extends RepositoryPrivileges> getRepositoryPrivileges();

    String getStorageId();

}
