package com.veadan.folib.authorization.dto;

import java.io.Serializable;
import java.util.LinkedHashSet;
import java.util.Set;

import com.fasterxml.jackson.annotation.JsonRootName;

/**
 * Java representation for authorization config that stored in YAML file.
 *
 * @author 
 * @author Veadan
 * @see /src/main/resources/etc/conf/folib-authorization.yaml
 * @see {@linkplain https://dev.carlspring.org/youtrack/issue/SB-126}
 */
@JsonRootName("authorizationConfiguration")
public class AuthorizationConfigDto
        implements Serializable
{

    private Set<RoleDto> roles = new LinkedHashSet<>();

    public Set<RoleDto> getRoles()
    {
        return roles;
    }

    public void setRoles(Set<RoleDto> roles)
    {
        this.roles = roles;
    }

    @Override
    public boolean equals(final Object o)
    {
        if (this == o) return true;
        if (!(o instanceof AuthorizationConfigDto))
        {
            return false;
        }
        final AuthorizationConfigDto config = (AuthorizationConfigDto) o;
        return java.util.Objects.equals(roles, config.roles);
    }

    @Override
    public String toString()
    {
        final StringBuilder sb = new StringBuilder("AuthorizationConfig{");
        sb.append("roles=").append(roles);
        sb.append('}');
        return sb.toString();
    }
}
