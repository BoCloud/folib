package com.folib.users.security;

import java.util.List;
import java.util.Set;

import com.folib.users.domain.Privileges;
import com.folib.users.dto.AccessModel;

/**
 * @author veadan
 *
 */
public class AdminAccessModel implements AccessModel
{
    
    @Override
    public Set<Privileges> getApiAuthorities()
    {
        return Privileges.all();
    }

    @Override
    public Set<Privileges> getPathAuthorities(String url)
    {
        return Privileges.all();
    }

    @Override
    public Set<Privileges> getPathAuthorities(String path, boolean enableSplitPath) {
        return Privileges.all();
    }

    @Override
    public Set<Privileges> getPathAuthorities(String storageId, String repositoryId, List<String> paths, boolean enableSplitPath) {
        return Privileges.all();
    }

    @Override
    public Set<Privileges> getPathAuthorities(String storageId, String repositoryId, List<String> paths) {
        return Privileges.all();
    }

}
