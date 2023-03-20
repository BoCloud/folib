package com.veadan.folib.controllers.users.support;

import java.io.Serializable;
import java.util.Collections;
import java.util.LinkedHashSet;
import java.util.Objects;
import java.util.Set;

import java.util.stream.Collectors;
import com.veadan.folib.domain.User;
import com.veadan.folib.domain.SecurityRole;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.google.common.collect.ImmutableSet;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.core.context.SecurityContextHolder;

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

    private String email;

    private String avatar;

    private Set<String> roles;

    private String securityTokenKey;

    private LinkedHashSet<String> authorities;

    public String getEmail() {
        return email;
    }

    public void setEmail(String email) {
        this.email = email;
    }

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
        output.setEmail(user.getEmail());
        output.setSecurityTokenKey(user.getSecurityTokenKey());
        Authentication authentication = SecurityContextHolder.getContext().getAuthentication();
        if (Objects.nonNull(authentication)) {
            output.setAuthorities(authentication.getAuthorities().stream()
                    .map(GrantedAuthority::getAuthority)
                    .sorted()
                    .collect(Collectors.toCollection(LinkedHashSet::new)));
        }
        return output;
    }

    public LinkedHashSet<String> getAuthorities() {
        return authorities;
    }

    public void setAuthorities(LinkedHashSet<String> authorities) {
        this.authorities = authorities;
    }

    @Override
    public String toString()
    {
        final StringBuilder sb = new StringBuilder("UserOutput{");
        sb.append("username='").append(username).append('\'');
        sb.append(", enabled=").append(enabled);
        sb.append(", roles=").append(roles);
        sb.append(", authorities=").append(authorities);
        sb.append(", securityTokenKey='").append(securityTokenKey).append('\'');
        sb.append('}');
        return sb.toString();
    }

    public String getAvatar() {
        return avatar;
    }

    public void setAvatar(String avatar) {
        this.avatar = avatar;
    }
}
