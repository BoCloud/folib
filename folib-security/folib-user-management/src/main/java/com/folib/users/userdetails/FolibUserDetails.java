package com.folib.users.userdetails;

import com.folib.domain.User;
import com.folib.domain.SecurityRole;

import java.util.Collection;
import java.util.stream.Collectors;

import lombok.extern.slf4j.Slf4j;

import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.core.authority.SimpleGrantedAuthority;
import org.springframework.security.core.userdetails.UserDetails;

@Slf4j
public class FolibUserDetails implements UserDetails
{

    private User user;

    public FolibUserDetails(User user)
    {
        this.user = user;
    }

    @Override
    public Collection<? extends GrantedAuthority> getAuthorities()
    {
        return user.getRoles()
                   .stream()
                   .map(SecurityRole::getRoleName)
                   .map(SimpleGrantedAuthority::new)
                   .collect(Collectors.toSet());
    }

    @Override
    public String getPassword()
    {
        return user.getPassword();
    }

    @Override
    public String getUsername()
    {
        return user.getUsername();
    }

    @Override
    public boolean isAccountNonExpired()
    {
        return true;
    }

    @Override
    public boolean isAccountNonLocked()
    {
        return true;
    }

    @Override
    public boolean isCredentialsNonExpired()
    {
        return true;
    }

    @Override
    public boolean isEnabled()
    {
        return user.isEnabled();
    }

    public User getUser()
    {
        return user;
    }

    @Override
    public String toString() {
        return "FolibUserDetails{" +
                "user=" + user +
                '}';
    }
}
