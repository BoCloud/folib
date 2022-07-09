package com.veadan.folib.controllers.users.support;

import java.io.Serializable;
import java.util.Collections;
import java.util.Set;

import java.util.stream.Collectors;
import com.veadan.folib.domain.User;
import com.veadan.folib.domain.SecurityRole;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.google.common.collect.ImmutableSet;

/**
 * @author veadan
 * @author Veadan
 * @JsonInclude used because com.veadan.folib.users.domain.User is annotated with it
 */
@JsonInclude(JsonInclude.Include.NON_NULL)
public class UserOutput
        implements Serializable
{

    private String username;

    private boolean enabled;

    private Set<String> roles;

    private String securityTokenKey;

    public String getUsername()
    {
        return username;
    }

    public void setUsername(String username)
    {
        this.username = username;
    }

    public boolean isEnabled()
    {
        return enabled;
    }

    public void setEnabled(boolean enabled)
    {
        this.enabled = enabled;
    }

    public Set<String> getRoles()
    {
        return roles == null ? Collections.emptySet() : ImmutableSet.copyOf(roles);
    }

    public void setRoles(Set<String> roles)
    {
        this.roles = roles;
    }

    public String getSecurityTokenKey()
    {
        return securityTokenKey;
    }

    public void setSecurityTokenKey(String securityTokenKey)
    {
        this.securityTokenKey = securityTokenKey;
    }

    public static UserOutput fromUser(User user)
    {
        final UserOutput output = new UserOutput();
        output.setEnabled(user.isEnabled());
        output.setRoles(user.getRoles()
                            .stream()
                            .map(SecurityRole::getRoleName)
                            .collect(Collectors.toSet()));
        output.setUsername(user.getUsername());
        output.setSecurityTokenKey(user.getSecurityTokenKey());
        return output;
    }

    @Override
    public String toString()
    {
        final StringBuilder sb = new StringBuilder("UserOutput{");
        sb.append("username='").append(username).append('\'');
        sb.append(", enabled=").append(enabled);
        sb.append(", roles=").append(roles);
        sb.append(", securityTokenKey='").append(securityTokenKey).append('\'');
        sb.append('}');
        return sb.toString();
    }
}
