package com.veadan.folib.domain;

import static com.veadan.folib.db.schema.Vertices.USER;
import static org.neo4j.ogm.annotation.Relationship.OUTGOING;
import static com.veadan.folib.db.schema.Edges.USER_HAS_SECURITY_ROLES;

import com.fasterxml.jackson.annotation.JsonFormat;
import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.veadan.folib.data.domain.DomainEntity;
import com.veadan.folib.gremlin.adapters.DateConverter;

import java.time.LocalDateTime;
import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;

import lombok.Data;
import lombok.EqualsAndHashCode;
import org.neo4j.ogm.annotation.NodeEntity;
import org.neo4j.ogm.annotation.Relationship;
import org.neo4j.ogm.annotation.typeconversion.Convert;

/**
 * @author xuxinping
 *
 */
@EqualsAndHashCode(callSuper = true)
@NodeEntity(USER)
public class UserEntity extends DomainEntity implements User
{

    public UserEntity(String name)
    {
        setUuid(name);
    }

    private String password;

    private String originalPassword;

    public void setGroupIds(Set<Long> groupIds) {
        this.groupIds = groupIds;
    }

    @Override
    public String getUsername() {
        return username;
    }

    public void setUsername(String username) {
        this.username = username;
    }

    public Boolean getEnabled() {
        return enabled;
    }

    private Boolean enabled = true;

    private String email;
    private String avatar;

    private String username;

    @Relationship(type = USER_HAS_SECURITY_ROLES, direction = OUTGOING)
    private Set<SecurityRole> roles = new HashSet<>();

    private Set<Long> groupIds = new HashSet<>();

    private Set<String> userGroups;
    private Set<String> userGroupIds;
    private String securityTokenKey;

    @Convert(DateConverter.class)
    @JsonDeserialize(using = LocalDateTimeDeserializer.class)
    private LocalDateTime lastUpdated;

    private String sourceId;

    private String userType="general";

    private String nickname;

    @Override
    public String getUserType() {
        return userType;
    }

    public void setUserType(String userType) {
        this.userType = userType;
    }

    UserEntity()
    {
    }

    @Override
    public String getEmail() {
        return email;
    }

    public void setEmail(String email) {
        this.email = email;
    }

    @Override
    public String getPassword()
    {
        return password;
    }

    public void setPassword(String password)
    {
        this.password = password;
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

    public void setRoles(Set<SecurityRole> roles)
    {
        this.roles = roles != null ? new HashSet<>(roles) : new HashSet<>();
    }

    public void addRole(String role)
    {
        addRole(new SecurityRoleEntity(role));
    }

    public void addRole(SecurityRole role)
    {
        roles.add(role);
    }

    public void removeRole(SecurityRole role)
    {
        roles.remove(role);
    }

    public boolean hasRole(SecurityRole role)
    {
        return roles.contains(role);
    }

    @Override
    public String getSecurityTokenKey()
    {
        return securityTokenKey;
    }

    public void setSecurityTokenKey(String securityTokenKey)
    {
        this.securityTokenKey = securityTokenKey;
    }

    @Override
    public Boolean isEnabled()
    {
        return enabled;
    }

    public void setEnabled(final Boolean enabled)
    {
        this.enabled = enabled;
    }

    @Override
    public LocalDateTime getLastUpdated()
    {
        return lastUpdated;
    }

    public void setLastUpdated(LocalDateTime lastUpdated)
    {
        this.lastUpdated = lastUpdated;
    }

    @Override
    public String getSourceId()
    {
        return sourceId;
    }

    public void setSourceId(String source)
    {
        this.sourceId = source;
    }

    @Override
    public String getAvatar() {
        return avatar;
    }

    public void setAvatar(String avatar) {
        this.avatar = avatar;
    }

    @Override
    public String getOriginalPassword() {
        return originalPassword;
    }

    public void setOriginalPassword(String originalPassword) {
        this.originalPassword = originalPassword;
    }

    @Override
    public Set<String> getUserGroups() {
        return userGroups;
    }

    public void setUserGroups(Set<String> userGroups) {
        this.userGroups = userGroups;
    }

    @Override
    public Set<String> getUserGroupIds() {
        return userGroupIds;
    }

    public void setUserGroupIds(Set<String> userGroupIds) {
        this.userGroupIds = userGroupIds;
    }

    public void setNickname(String nickname) {
        this.nickname = nickname;
    }
    @Override
    public String getNickname() {
        return this.nickname;
    }
}
