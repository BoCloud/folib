package com.folib.users.domain;

import java.io.Serializable;
import java.time.LocalDateTime;
import java.util.Collections;
import java.util.HashSet;
import java.util.Set;
import java.util.stream.Collectors;

import javax.annotation.concurrent.Immutable;

import com.folib.domain.User;
import com.folib.domain.SecurityRole;
import com.folib.domain.SecurityRoleEntity;
import com.folib.users.dto.UserDto;
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

    private final String originalPassword;

    private final String email;

    private String avatar;

    private final String userType;

    private final Boolean enabled;

    private final Set<SecurityRole> roles;

    private Set<Long> groupIds = new HashSet<>();
    private Set<String> userGroups = new HashSet<>();
    private Set<String> userGroupIds = new HashSet<>();
    private final String securityTokenKey;

    private final LocalDateTime lastUpdate;

    private String sourceId;

    private String nickname;

    public UserData(final UserDetails source)
    {
        this.username = source.getUsername();
        this.password = source.getPassword();
        this.originalPassword = null;
        this.enabled = source.isEnabled();
        this.roles = source.getAuthorities()
                           .stream()
                           .map(a -> new SecurityRoleEntity(a.getAuthority()))
                           .collect(Collectors.toSet());
        this.securityTokenKey = null;
        this.lastUpdate = null;
        this.email=null;
        this.userType=null;
        this.avatar = null;
        this.nickname = null;
    }

    public UserData(final UserDto source)
    {
        this.username = source.getUsername();
        this.password = source.getPassword();
        this.originalPassword = source.getOriginalPassword();
        this.enabled = source.isEnabled();
        this.roles = immuteRoles(source.getRoles());
        this.securityTokenKey = source.getSecurityTokenKey();
        this.lastUpdate = source.getLastUpdated();
        this.sourceId = source.getSourceId();
        this.email=source.getEmail();
        this.userType=source.getUserType();
        this.avatar = source.getAvatar();
        this.nickname = source.getNickname();
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
    public String getEmail() {
        return email;
    }

    @Override
    public String getUserType() {
        return userType;
    }

    @Override
    public String getPassword()
    {
        return password;
    }

    @Override
    public String getOriginalPassword()
    {
        return originalPassword;
    }

    @Override
    public Set<SecurityRole> getRoles()
    {
        return roles;
    }

    @Override
    public Set<Long> getGroupIds() {
        return groupIds != null ? new HashSet<>(groupIds)
                : new HashSet<>();

    }

    @Override
    public Set<String> getUserGroups() {
        return userGroups;
    }

    @Override
    public Set<String> getUserGroupIds() {
        return userGroupIds;
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
    public String getAvatar() {
        return avatar;
    }

    @Override
    public String getNickname() {
        return this.nickname;
    }

    @Override
    public String toString() {
        return "UserData{" +
                "username='" + username + '\'' +
                ", password='" + password + '\'' +
                ", email='" + email + '\'' +
                ", userType='" + userType + '\'' +
                ", enabled=" + enabled +
                ", roles=" + roles +
                ", securityTokenKey='" + securityTokenKey + '\'' +
                ", lastUpdate=" + lastUpdate +
                ", sourceId='" + sourceId + '\'' +
                '}';
    }
}
