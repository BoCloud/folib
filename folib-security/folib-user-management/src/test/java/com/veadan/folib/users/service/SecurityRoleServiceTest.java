package com.veadan.folib.users.service;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

import com.veadan.folib.config.DataServiceConfig;
import com.veadan.folib.config.UsersConfig;
import com.veadan.folib.users.domain.SystemRole;
import com.veadan.folib.domain.SecurityRole;
import com.veadan.folib.domain.SecurityRoleEntity;
import com.veadan.folib.repositories.SecurityRoleRepository;

import javax.inject.Inject;

import org.junit.jupiter.api.Test;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.context.ContextConfiguration;

/**
 * @author ankit.tomar
 */
@SpringBootTest
@ActiveProfiles(profiles = "test")
@ContextConfiguration(classes = { DataServiceConfig.class,
                                  UsersConfig.class })
public class SecurityRoleServiceTest
{

    @Inject
    private SecurityRoleRepository securityRoleRepository;

    @Inject
    private SecurityRoleService userRoleService;

    @Test
    public void testFetchExistingRole()
    {
        securityRoleRepository.save(new SecurityRoleEntity(SystemRole.ADMIN.name()));

        SecurityRole userRolefromDB = userRoleService.findOneOrCreate(SystemRole.ADMIN.name());
        assertNotNull(userRolefromDB);
        assertEquals(userRolefromDB.getRoleName(), SystemRole.ADMIN.name());
    }

    @Test
    public void testFetchNonExistingRole()
    {
        SecurityRole userRolefromDB = userRoleService.findOneOrCreate(SystemRole.REPOSITORY_MANAGER.name());
        assertNotNull(userRolefromDB);
        assertEquals(userRolefromDB.getRoleName(), SystemRole.REPOSITORY_MANAGER.name());
    }
}
