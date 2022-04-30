package com.veadan.folib.users.service;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

import com.veadan.folib.config.DataServiceConfig;
import com.veadan.folib.config.UsersConfig;
import com.veadan.folib.domain.UserEntity;
import com.veadan.folib.domain.SecurityRoleEntity;
import com.veadan.folib.users.domain.SystemRole;
import com.veadan.folib.users.userdetails.SpringSecurityUser;
import com.veadan.folib.users.userdetails.UserDetailsMapper;

import javax.inject.Inject;

import org.junit.jupiter.api.Test;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.context.ContextConfiguration;

import com.google.common.collect.Sets;

/**
 * @author ankit.tomar
 */
@SpringBootTest
@ActiveProfiles(profiles = "test")
@ContextConfiguration(classes = { DataServiceConfig.class,
                                  UsersConfig.class })
public class UserDetailsMapperTest
{

    @Inject
    private UserDetailsMapper userDetailsMapper;

    @Test
    public void testEncodedPasswordUserWithPasswordEncodingAlgoPrefix()
    {
        UserEntity user = new UserEntity("test-user");
        user.setPassword("{bcrypt}$2a$10$WqtVx7Iio0cndyR1lEaKW.SWhUYmF/zHHG5hkAXvH5hUmklM7QfMO");
        user.setRoles(Sets.newHashSet(new SecurityRoleEntity(SystemRole.REPOSITORY_MANAGER.name())));
        user.setEnabled(true);
        SpringSecurityUser securityUser = userDetailsMapper.apply(user);
        assertNotNull(securityUser);
        assertEquals(securityUser.getUsername(), "test-user");
        assertEquals(securityUser.getPassword(),
                     "{bcrypt}$2a$10$WqtVx7Iio0cndyR1lEaKW.SWhUYmF/zHHG5hkAXvH5hUmklM7QfMO");
        assertNotNull(securityUser.getRoles());
    }

    @Test
    public void testEncodedPasswordUserWithoutPasswordEncodingAlgoPrefix()
    {
        UserEntity user = new UserEntity("test-user");
        user.setPassword("$2a$10$WqtVx7Iio0cndyR1lEaKW.SWhUYmF/zHHG5hkAXvH5hUmklM7QfMO");
        user.setRoles(Sets.newHashSet(new SecurityRoleEntity(SystemRole.REPOSITORY_MANAGER.name())));
        user.setEnabled(true);
        SpringSecurityUser securityUser = userDetailsMapper.apply(user);
        assertNotNull(securityUser);
        assertEquals(securityUser.getUsername(), "test-user");
        assertEquals(securityUser.getPassword(),
                     "{bcrypt}$2a$10$WqtVx7Iio0cndyR1lEaKW.SWhUYmF/zHHG5hkAXvH5hUmklM7QfMO");
        assertNotNull(securityUser.getRoles());
    }

}
