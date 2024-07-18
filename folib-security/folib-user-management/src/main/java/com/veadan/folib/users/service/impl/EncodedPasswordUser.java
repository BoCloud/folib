package com.veadan.folib.users.service.impl;

import com.veadan.folib.domain.SecurityRole;
import com.veadan.folib.domain.User;
import org.apache.commons.lang3.StringUtils;
import org.springframework.security.crypto.password.PasswordEncoder;

import java.time.LocalDateTime;
import java.util.HashSet;
import java.util.Optional;
import java.util.Set;

/**
 * @author xuxinping
 */
public class EncodedPasswordUser implements User {

    private final User user;
    private final PasswordEncoder passwordEncoder;
    private Set<Long> groupIds = new HashSet<>();
    public EncodedPasswordUser(User user,
                               PasswordEncoder passwordEncoder) {
        this.user = user;
        this.passwordEncoder = passwordEncoder;
    }

    @Override
    public String getUuid() {
        return getUsername();
    }

    @Override
    public String getUsername() {
        return user.getUsername();
    }

    @Override
    public String getEmail() {
        return user.getEmail();
    }

    @Override
    public String getUserType() {
        return user.getUserType();
    }

    @Override
    public String getPassword() {
        String password = user.getPassword();

        return Optional.ofNullable(password)
                .filter(p -> StringUtils.isNoneBlank(p))
                .map(p -> passwordEncoder.encode(p))
                .orElse(password);
    }


    @Override
    public String getOriginalPassword() {
        return user.getOriginalPassword();
    }

    @Override
    public Set<SecurityRole> getRoles() {
        return user.getRoles();
    }

    @Override
    public Set<Long> getGroupIds() {
        return groupIds != null ? new HashSet<>(groupIds)
                : new HashSet<>();

    }

    @Override
    public String getSecurityTokenKey() {
        return user.getSecurityTokenKey();
    }

    @Override
    public Boolean isEnabled() {
        return user.isEnabled();
    }

    @Override
    public LocalDateTime getLastUpdated() {
        return user.getLastUpdated();
    }

    @Override
    public String getSourceId() {
        return user.getSourceId();
    }

    @Override
    public String getAvatar() {
        return user.getAvatar();
    }

}
