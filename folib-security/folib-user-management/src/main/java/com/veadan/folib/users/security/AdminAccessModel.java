package com.veadan.folib.users.security;

import java.util.List;
import java.util.Set;

import com.veadan.folib.users.domain.Privileges;
import com.veadan.folib.users.dto.AccessModel;

/**
 * @author xuxinping
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
    public Set<Privileges> getPathAuthorities(String storageId, String repositoryId, List<String> paths) {
        return Privileges.all();
    }

}
