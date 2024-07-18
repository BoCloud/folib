package com.veadan.folib.forms.users;

import com.veadan.folib.validation.users.Password;
import com.veadan.folib.validation.users.UniqueUsername;

import javax.validation.constraints.NotEmpty;
import java.io.Serializable;
import java.util.Collections;
import java.util.Set;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.google.common.collect.ImmutableSet;

/**
 * @author Veadan
 */
@JsonIgnoreProperties(ignoreUnknown = true)
public class UserForm
        implements Serializable
{

    @NotEmpty(groups = { NewUser.class }, message = "Username is required!")
    @UniqueUsername(groups = NewUser.class, message = "Username is already taken.")
    private String username;

    @Password(groups = { NewUser.class }, min = 8)
    @Password(groups = { ExistingUser.class }, allowNull = true, min = 8)
    private String password;

    private String email;

    private String avatar;

    private boolean enabled;

    private Set<String> roles;
    private Set<Long> groupIds;

    private String securityTokenKey;

    public String getUsername()
    {
        return username;
    }

    public void setUsername(String username)
    {
        this.username = username;
    }

    public String getPassword()
    {
        return password;
    }

    public void setPassword(String password)
    {
        this.password = password;
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


    public String getEmail() {
        return email;
    }

    public void setEmail(String email) {
        this.email = email;
    }

    public interface NewUser
            extends Serializable
    {
        // validation group marker interface for new users.
    }

    public interface ExistingUser
            extends Serializable
    {
        // validation group marker interface for existing users.
    }

    public interface UpdateAccount
            extends Serializable
    {
        // validation group marker interface for existing users.
    }

    public String getAvatar() {
        return avatar;
    }

    public void setAvatar(String avatar) {
        this.avatar = avatar;
    }
}

