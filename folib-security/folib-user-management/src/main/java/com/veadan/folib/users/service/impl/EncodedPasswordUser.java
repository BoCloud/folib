package com.veadan.folib.users.service.impl;

import java.time.LocalDateTime;
import java.util.Optional;
import java.util.Set;

import org.apache.commons.lang3.StringUtils;
import com.veadan.folib.domain.User;
import com.veadan.folib.domain.SecurityRole;

import org.springframework.security.crypto.password.PasswordEncoder;

/**
 * @author xuxinping
 *
 */
public class EncodedPasswordUser implements User
{

    private final User user;
    private final PasswordEncoder passwordEncoder;

    public EncodedPasswordUser(User user,
                               PasswordEncoder passwordEncoder)
    {
        this.user = user;
        this.passwordEncoder = passwordEncoder;
    }

    @Override
    public String getUuid()
    {
        return getUsername();
    }
    
    public String getUsername()
    {
        return user.getUsername();
    }

    @Override
    public String getEmail() {
        return user.getEmail();
    }

    public String getPassword()
    {
        String password = user.getPassword();

        return Optional.ofNullable(password)
                       .filter(p -> StringUtils.isNoneBlank(p))
                       .map(p -> passwordEncoder.encode(p))
                       .orElse(password);
    }

    public Set<SecurityRole> getRoles()
    {
        return user.getRoles();
    }

    public String getSecurityTokenKey()
    {
        return user.getSecurityTokenKey();
    }

    public Boolean isEnabled()
    {
        return user.isEnabled();
    }

    public LocalDateTime getLastUpdated()
    {
        return user.getLastUpdated();
    }

    public String getSourceId()
    {
        return user.getSourceId();
    }
    
}
