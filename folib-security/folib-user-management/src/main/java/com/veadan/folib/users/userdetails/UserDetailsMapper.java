package com.veadan.folib.users.userdetails;

import com.veadan.folib.users.security.AuthoritiesProvider;

import javax.inject.Inject;

import java.util.stream.Collectors;

import com.veadan.folib.domain.User;
import com.veadan.folib.domain.SecurityRole;

import org.springframework.stereotype.Component;

@Component
public class UserDetailsMapper implements FolibUserToUserDetails
{

    @Inject
    private AuthoritiesProvider authoritiesProvider;

    @Override
    public SpringSecurityUser apply(User user)
    {

        SpringSecurityUser springUser = new SpringSecurityUser();
        springUser.setEnabled(user.isEnabled());
        springUser.setEmail(user.getEmail());
        springUser.setUserType("general");
        springUser.setPassword(getPasswordWithEncodingAlgorithm(user.getPassword()));
        springUser.setUsername(user.getUsername());
        springUser.setRoles(user.getRoles()
                                .stream()
                                .map(SecurityRole::getRoleName)
                                .map(authoritiesProvider::getRuntimeRole)
                                .collect(Collectors.toSet()));
        springUser.setSecurityKey(user.getSecurityTokenKey());
        springUser.setSourceId(user.getSourceId());

        return springUser;
    }

    private String getPasswordWithEncodingAlgorithm(String password)
    {
        String algorithmPrefix = extractAlgorithmPrefix(password);
        if (algorithmPrefix == null)
        {
            return "{bcrypt}" + password;
        }
        return password;
    }

    private String extractAlgorithmPrefix(String prefixEncodedPassword)
    {
        if (prefixEncodedPassword == null)
        {
            return null;
        }
        int start = prefixEncodedPassword.indexOf("{");
        if (start != 0)
        {
            return null;
        }
        int end = prefixEncodedPassword.indexOf("}", start);
        if (end < 0)
        {
            return null;
        }
        return prefixEncodedPassword.substring(start + 1, end);
    }

}
