package com.veadan.folib.dto.users;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.google.common.collect.ImmutableSet;
import com.veadan.folib.validation.users.Password;
import com.veadan.folib.validation.users.UniqueUsername;

import javax.validation.constraints.NotEmpty;
import java.io.Serializable;
import java.util.Collections;
import java.util.Set;

/**
 * @author Veadan
 */
@JsonIgnoreProperties(ignoreUnknown = true)
public class UserDto
        implements Serializable {

    @NotEmpty(groups = {NewUser.class}, message = "Username is required!")
    @UniqueUsername(groups = NewUser.class, message = "Username is already taken.")
    private String username;

    @Password(groups = {NewUser.class}, allowNull = true, min = 12)
    @Password(groups = {ExistingUser.class}, allowNull = true, min = 12)
    private String password;

    @Password(groups = {NewUser.class}, allowNull = true, min = 12)
    @Password(groups = {ExistingUser.class}, allowNull = true, min = 12)
    private String originalPassword;

    private String email;

    private String avatar;

    private boolean enabled;

    private String nickname;

    private Set<String> roles;
    private Set<String> userGroupIds;

    private String securityTokenKey;

    public String getUsername() {
        return username;
    }

    public void setUsername(String username) {
        this.username = username;
    }

    public String getPassword() {
        return password;
    }

    public void setPassword(String password) {
        this.password = password;
    }

    public boolean isEnabled() {
        return enabled;
    }

    public void setEnabled(boolean enabled) {
        this.enabled = enabled;
    }

    public Set<String> getRoles() {
        return roles == null ? Collections.emptySet() : ImmutableSet.copyOf(roles);
    }

    public void setRoles(Set<String> roles) {
        this.roles = roles;
    }

    public String getSecurityTokenKey() {
        return securityTokenKey;
    }

    public void setSecurityTokenKey(String securityTokenKey) {
        this.securityTokenKey = securityTokenKey;
    }


    public String getEmail() {
        return email;
    }

    public void setEmail(String email) {
        this.email = email;
    }

    public Set<String> getUserGroupIds() {
        return userGroupIds;
    }

    public void setUserGroupIds(Set<String> userGroupIds) {
        this.userGroupIds = userGroupIds;
    }

    public interface NewUser
            extends Serializable {
        // validation group marker interface for new users.
    }

    public interface ExistingUser
            extends Serializable {
        // validation group marker interface for existing users.
    }

    public interface UpdateAccount
            extends Serializable {
        // validation group marker interface for existing users.
    }

    public String getNickname() {
        return nickname;
    }

    public void setNickname(String nickname) {
        this.nickname = nickname;
    }

    public String getAvatar() {
        return avatar;
    }

    public void setAvatar(String avatar) {
        this.avatar = avatar;
    }

    public String getOriginalPassword() {
        return originalPassword;
    }

    public void setOriginalPassword(String originalPassword) {
        this.originalPassword = originalPassword;
    }
}

