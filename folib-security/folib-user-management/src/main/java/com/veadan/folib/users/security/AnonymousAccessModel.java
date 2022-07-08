package com.veadan.folib.users.security;

import java.util.Collections;
import java.util.HashSet;
import java.util.Set;

import com.veadan.folib.users.domain.Privileges;
import com.veadan.folib.users.dto.AccessModel;


/**
 * @author xuxinping
 *
 */
public class AnonymousAccessModel implements AccessModel
{
    
    private final AccessModel target;
    
    public AnonymousAccessModel(AccessModel target)
    {
        this.target = target;
    }

    public Set<Privileges> getApiAuthorities()
    {
        Set<Privileges> authorities = new HashSet<Privileges>(target.getApiAuthorities());
        authorities.add(Privileges.ANONYMOUS_USER);
        
        return Collections.unmodifiableSet(authorities);
    }

    @Override
    public Set<Privileges> getPathAuthorities(String url)
    {
        return target.getPathAuthorities(url);
    }

}
