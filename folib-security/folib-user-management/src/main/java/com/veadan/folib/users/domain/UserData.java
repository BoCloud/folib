package com.veadan.folib.users.domain;

import java.io.Serializable;
import java.time.LocalDateTime;
import java.util.Collections;
import java.util.Set;
import java.util.stream.Collectors;

import javax.annotation.concurrent.Immutable;

import com.veadan.folib.domain.User;
import com.veadan.folib.domain.SecurityRole;
import com.veadan.folib.domain.SecurityRoleEntity;
import com.veadan.folib.users.dto.UserDto;
import org.springframework.security.core.userdetails.UserDetails;

import com.google.common.collect.ImmutableSet;

/**
 * @author veadan
 */
@Immutable
public class UserData implements Serializable, User
{

    public static final String SECURITY_TOKEN_KEY = "security-token-key";

    private final String username;

    private final String password;

    private final Boolean enabled;

    private final Set<SecurityRole> roles;

    private final String securityTokenKey;

    private final LocalDateTime lastUpdate;

    private String sourceId;

    public UserData(final UserDetails source)
    {
        this.username = source.getUsername();
        this.password = source.getPassword();
        this.enabled = source.isEnabled();
        this.roles = source.getAuthorities()
                           .stream()
                           .map(a -> new SecurityRoleEntity(a.getAuthority()))
                           .collect(Collectors.toSet());
        this.securityTokenKey = null;
        this.lastUpdate = null;
    }

    public UserData(final UserDto source)
    {
        this.username = source.getUsername();
        this.password = source.getPassword();
        this.enabled = source.isEnabled();
        this.roles = immuteRoles(source.getRoles());
        this.securityTokenKey = source.getSecurityTokenKey();
        this.lastUpdate = source.getLastUpdated();
        this.sourceId = source.getSourceId();
    }

    private Set<SecurityRole> immuteRoles(final Set<SecurityRole> source)
    {
        return source != null ? ImmutableSet.copyOf(source) : Collections.emptySet();
    }

    @Override
    public String getUuid()
    {
        return getUsername();
    }

    @Override
    public String getUsername()
    {
        return username;
    }

    @Override
    public String getPassword()
    {
        return password;
    }

    @Override
    public Set<SecurityRole> getRoles()
    {
        return roles;
    }

    @Override
    public String getSecurityTokenKey()
    {
        return securityTokenKey;
    }

    @Override
    public Boolean isEnabled()
    {
        return enabled;
    }

    @Override
    public LocalDateTime getLastUpdated()
    {
        return lastUpdate;
    }

    @Override
    public String getSourceId()
    {
        return sourceId;
    }

    @Override
    public String toString()
    {
        final StringBuilder sb = new StringBuilder("User{");
        sb.append("username='")
          .append(username)
          .append('\'');
        sb.append(", roles=")
          .append(roles);
        sb.append('}');
        return sb.toString();
    }
}
