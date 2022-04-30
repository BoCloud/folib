package com.veadan.folib.users.dto;

import java.util.Set;

import com.veadan.folib.users.domain.Privileges;

public interface RepositoryPrivileges
{

    String getRepositoryId();

    Set<Privileges> getRepositoryPrivileges();

    Set<? extends PathPrivileges> getPathPrivileges();

}
