package com.veadan.folib.repositories;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

import com.veadan.folib.config.DataServiceConfig;
import com.veadan.folib.config.UsersConfig;
import com.veadan.folib.users.domain.SystemRole;
import com.veadan.folib.domain.User;
import com.veadan.folib.domain.UserEntity;
import com.veadan.folib.domain.SecurityRole;
import com.veadan.folib.domain.SecurityRoleEntity;
import com.veadan.folib.gremlin.dsl.EntityTraversalUtils;

import javax.inject.Inject;
import javax.transaction.Transactional;

import java.util.Date;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

import org.junit.jupiter.api.BeforeEach;
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
public class UserRepositoryTest
{

    @Inject
    private UserRepository userRepository;

    @BeforeEach
    public void before()
    {
        saveUserWithRoles("test-user1", Sets.newHashSet(SystemRole.ADMIN.name(), SystemRole.ARTIFACTS_MANAGER.name()));
        saveUserWithRoles("test-user2", Sets.newHashSet(SystemRole.ADMIN.name(), "CUSTOM_ROLE"));
    }

    @Test
    @Transactional
    public void testFindAllUsers()
    {
        List<User> findAllUsers = userRepository.findAllUsers();
        assertNotNull(findAllUsers);
        assertEquals(2, findAllUsers.size());
        assertEquals(2, findAllUsers.iterator().next().getRoles().size());
        assertEquals(2, findAllUsers.iterator().next().getRoles().size());
    }

    @Test
    @Transactional
    public void testFindUsersWithRoleNotExist()
    {
        List<User> usersWithRole = userRepository.findUsersWithRole(SystemRole.REPOSITORY_MANAGER.name());
        assertNotNull(usersWithRole);
        assertEquals(0, usersWithRole.size());
    }

    @Test
    @Transactional
    public void testFindUsersWithRoleExist()
    {
        List<User> usersWithRole = userRepository.findUsersWithRole(SystemRole.ADMIN.name());
        assertNotNull(usersWithRole);
        assertEquals(2, usersWithRole.size());

        usersWithRole = userRepository.findUsersWithRole("CUSTOM_ROLE");
        assertNotNull(usersWithRole);
        assertEquals(1, usersWithRole.size());
    }

    private void saveUserWithRoles(String username,
                                   Set<String> roles)
    {
        UserEntity user = new UserEntity(username);
        user.setPassword("password");
        user.setSecurityTokenKey("security-token");
        Set<SecurityRole> userRoles = roles.stream()
                                       .map(SecurityRoleEntity::new)
                                       .collect(Collectors.toSet());
        user.setRoles(userRoles);
        user.setEnabled(true);
        user.setLastUpdated(EntityTraversalUtils.toLocalDateTime(new Date()));
        userRepository.save(user);
    }
}
