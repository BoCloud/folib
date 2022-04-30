package com.veadan.folib.users.security;

import java.util.Set;

import com.veadan.folib.users.domain.Privileges;
import com.veadan.folib.users.dto.AccessModel;

/**
 * @author sbespalov
 *
 */
public class AdminAccessModel implements AccessModel
{
    
    public Set<Privileges> getApiAuthorities()
    {
        return Privileges.all();
    }

    @Override
    public Set<Privileges> getPathAuthorities(String url)
    {
        return Privileges.all();
    }

}
