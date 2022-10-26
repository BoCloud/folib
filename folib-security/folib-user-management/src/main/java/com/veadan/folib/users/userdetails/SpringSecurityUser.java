package com.veadan.folib.users.userdetails;

import com.beust.jcommander.internal.Sets;
import com.google.common.base.Objects;
import com.veadan.folib.authorization.dto.Role;
import com.veadan.folib.authorization.dto.RoleDto;
import com.veadan.folib.users.domain.Privileges;
import org.apache.commons.collections4.CollectionUtils;
import org.springframework.security.core.userdetails.UserDetails;

import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Set;
import java.util.stream.Collectors;

public class SpringSecurityUser
        implements UserDetails {

    private String username;

    private String password;

    private String email;

    private String userType = "general";

    private Boolean enabled;

    private Set<Role> roles = Collections.emptySet();

    private String url;

    private String securityKey;

    private String sourceId;

    @Override
    public String getUsername() {
        return username;
    }

    public void setUsername(String username) {
        this.username = username;
    }

    @Override
    public boolean isAccountNonExpired() {
        return Boolean.TRUE.equals(enabled);
    }

    @Override
    public boolean isAccountNonLocked() {
        return Boolean.TRUE.equals(enabled);
    }

    @Override
    public boolean isCredentialsNonExpired() {
        return Boolean.TRUE.equals(enabled);
    }

    @Override
    public String getPassword() {
        return password;
    }

    public void setPassword(String password) {
        this.password = password;
    }

    @Override
    public boolean isEnabled() {
        return Boolean.TRUE.equals(enabled);
    }

    public void setEnabled(Boolean enabled) {
        this.enabled = enabled;
    }

    public Set<Role> getRoles() {
        return roles;
    }

    public void setRoles(Set<Role> roles) {
        this.roles = roles;
    }

    public String getEmail() {
        return email;
    }

    public void setEmail(String email) {
        this.email = email;
    }

    public String getUserType() {
        return userType;
    }

    public void setUserType(String userType) {
        this.userType = userType;
    }

    @Override
    public Collection<Privileges> getAuthorities() {
        return roles.stream().flatMap(r -> r.getAccessModel().getApiAuthorities().stream()).collect(Collectors.toSet());
    }

    public Collection<Privileges> getStorageAuthorities(String path) {
        return getRoles().stream()
                .flatMap(r -> r.getAccessModel()
                        .getPathAuthorities(path)
                        .stream())
                .collect(Collectors.toSet());
    }

    public String getUrl() {
        return url;
    }

    public void setUrl(String url) {
        this.url = url;
    }

    public String getSecurityKey() {
        return securityKey;
    }

    public void setSecurityKey(String securityKey) {
        this.securityKey = securityKey;
    }

    public String getSourceId() {
        return sourceId;
    }

    public void setSourceId(String sourceId) {
        this.sourceId = sourceId;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o)
            return true;
        if (o == null || getClass() != o.getClass())
            return false;
        SpringSecurityUser user = (SpringSecurityUser) o;
        return enabled.equals(user.enabled) &&
                Objects.equal(username, user.username) &&
                Objects.equal(password, user.password) &&
                Objects.equal(roles, user.roles) &&
                Objects.equal(url, user.url) &&
                Objects.equal(securityKey, user.securityKey) &&
                Objects.equal(sourceId, user.sourceId);
    }

    @Override
    public int hashCode() {
        String[] hashCodeTargets = new String[roles.size() + 6];
        int i = 0;
        for (Role role : roles) {
            hashCodeTargets[i++] = role.getName();
        }
        hashCodeTargets[i++] = String.valueOf(username);
        hashCodeTargets[i++] = String.valueOf(password);
        hashCodeTargets[i++] = String.valueOf(enabled);
        hashCodeTargets[i++] = String.valueOf(securityKey);
        hashCodeTargets[i++] = String.valueOf(url);
        hashCodeTargets[i++] = String.valueOf(sourceId);

        Arrays.sort(hashCodeTargets);

        return Arrays.hashCode(hashCodeTargets);
    }

    @Override
    public String toString() {
        String role = CollectionUtils.isNotEmpty(roles) ? roles.stream().map(Role::getName).collect(Collectors.joining(",")) : "";
        return "SpringSecurityUser{" +
                "username='" + username + '\'' +
                ", password='" + password + '\'' +
                ", email='" + email + '\'' +
                ", userType='" + userType + '\'' +
                ", enabled=" + enabled +
                ", roles='" + role + '\'' +
                ", url='" + url + '\'' +
                ", securityKey='" + securityKey + '\'' +
                ", sourceId='" + sourceId + '\'' +
                '}';
    }
}
