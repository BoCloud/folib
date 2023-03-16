package com.veadan.folib.users.dto;

import java.io.Serializable;
import java.util.Set;

import com.veadan.folib.users.domain.Privileges;

public interface AccessModel extends Serializable
{
    Set<Privileges> getApiAuthorities();

    Set<Privileges> getPathAuthorities(String path);

    Set<Privileges> getPathAuthorities(String storageId, String repositoryId);
}
