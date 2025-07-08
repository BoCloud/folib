package com.folib.users.dto;

import java.io.Serializable;
import java.util.List;
import java.util.Set;

import com.folib.users.domain.Privileges;

public interface AccessModel extends Serializable
{
    Set<Privileges> getApiAuthorities();

    Set<Privileges> getPathAuthorities(String path);

    Set<Privileges> getPathAuthorities(String path, boolean enableSplitPath);

    Set<Privileges> getPathAuthorities(String storageId, String repositoryId, List<String> paths, boolean enableSplitPath);

    Set<Privileges> getPathAuthorities(String storageId, String repositoryId, List<String> paths);
}
