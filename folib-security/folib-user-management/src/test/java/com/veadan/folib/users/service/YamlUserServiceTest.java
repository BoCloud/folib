package com.veadan.folib.users.service;

import java.util.Arrays;
import java.util.HashSet;

import javax.inject.Inject;

import com.veadan.folib.config.DataServiceConfig;
import com.veadan.folib.config.UsersConfig;
import com.veadan.folib.domain.User;
import com.veadan.folib.users.dto.UserDto;
import com.veadan.folib.users.service.impl.EncodedPasswordUser;
import com.veadan.folib.users.service.impl.YamlUserService.Yaml;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.context.ContextConfiguration;
import static org.assertj.core.api.Assertions.assertThat;

@SpringBootTest
@ActiveProfiles(profiles = "test")
@ContextConfiguration(classes = { DataServiceConfig.class,
                                  UsersConfig.class })
public class YamlUserServiceTest
{

    private static final Logger logger = LoggerFactory.getLogger(YamlUserServiceTest.class);

    @Inject
    @Yaml
    UserService userService;

    @Inject
    private PasswordEncoder passwordEncoder;
    
    @BeforeEach
    public void setup()
    {
        assertThat(userService).isNotNull();
    }

    @Test
    public void testFindByUsername()
    {
        // Load the user
        User user = userService.findByUsername("deployer");
        assertThat(user).as("Unable to find user by name test-user").isNotNull();

        User nullUser = userService.findByUsername(null);
        assertThat(nullUser).as("User should have been null").isNull();
    }

    @Test
    public void testCreate()
            throws Exception
    {
        String testUserName = "test-user";

        UserDto user = new UserDto();
        user.setEnabled(true);
        user.setUsername(testUserName);
        user.setPassword("test-password");
        user.setSecurityTokenKey("some-security-token");

        userService.save(new EncodedPasswordUser(user, passwordEncoder));

        User foundEntity = userService.findByUsername(testUserName);

        assertThat(foundEntity).as("Unable to locate user " + testUserName + ". Save operation failed!").isNotNull();

        logger.debug("Found stored user\n\t{}\n", foundEntity);

        assertThat(foundEntity.getUsername()).isEqualTo(testUserName);
        assertThat(foundEntity.getPassword())
                .as("Expected a hashed password, received plain-text!")
                .isNotEqualTo("test-password"); // password should NOT be saved as "plain"
        assertThat(foundEntity.getPassword()).as("User contains empty password!").isNotNull();
        assertThat(foundEntity.isEnabled()).isTrue();
        assertThat(foundEntity.getSecurityTokenKey()).isEqualTo("some-security-token");
    }

    @Test
    public void testUpdateUser()
            throws Exception
    {
        String testUserName = "test-update-user";

        UserDto userAdd = new UserDto();
        userAdd.setEnabled(true);
        userAdd.setUsername(testUserName);
        userAdd.setPassword("test-password");
        userAdd.setSecurityTokenKey("before");

        userService.save(userAdd);

        User addedEntity = userService.findByUsername(testUserName);
        assertThat(addedEntity).as("Unable to locate user " + testUserName + ". Save operation failed!").isNotNull();

        logger.debug("Found stored user\n\t{}\n", addedEntity);

        logger.debug("Updating user...");

        UserDto userUpdate = new UserDto();
        userUpdate.setUsername(testUserName);
        userUpdate.setPassword("another-password");
        userUpdate.setSecurityTokenKey("after");
        userUpdate.setEnabled(false);

        userService.save(userUpdate);

        User updatedEntity = userService.findByUsername(testUserName);
        assertThat(updatedEntity).as("Unable to locate updated user " + testUserName + ". Update operation failed!").isNotNull();

        logger.debug("Found stored updated user\n\t{}\n", updatedEntity);

        assertThat(updatedEntity.getUsername()).isEqualTo(testUserName);
        assertThat(updatedEntity.getPassword())
                .as("Expected current password to have changed.")
                .isNotEqualTo(addedEntity.getPassword());
        assertThat(updatedEntity.getPassword()).as("Expected password to be other than null").isNotNull();
        assertThat(updatedEntity.isEnabled()).as("User should have been disabled, but is still enabled!").isFalse();
        assertThat(updatedEntity.getSecurityTokenKey()).isEqualTo("after");
    }


    @Test
    public void testUpdatingUserWithEmptyAndBlankPasswordShouldNotUpdatePasswordField()
            throws Exception
    {

        // 1. add initial user
        String testUserName = "test-update-user-empty-blank-pass";

        UserDto userAdd = new UserDto();
        userAdd.setEnabled(true);
        userAdd.setUsername(testUserName);
        userAdd.setPassword("initial");

        userService.save(userAdd);

        User addedEntity = userService.findByUsername(testUserName);
        assertThat(addedEntity).as("Unable to locate user " + testUserName + ". Save operation failed!").isNotNull();

        logger.debug("Found stored initial user\n\t{}\n", addedEntity);

        // 2. Update the user with empty/null password
        logger.debug("Updating user with empty/null pass...");

        UserDto userNullPassUpdate = new UserDto();
        userNullPassUpdate.setUsername(testUserName);
        userNullPassUpdate.setPassword(null);

        userService.save(userNullPassUpdate);

        User updatedEntity = userService.findByUsername(testUserName);
        assertThat(updatedEntity).as("Unable to locate updated user " + testUserName + ". Update operation failed!").isNotNull();

        logger.debug("Found stored updated with empty pass user\n\t{}\n", updatedEntity);

        assertThat(updatedEntity.getPassword()).as("User password has changed!").isEqualTo(addedEntity.getPassword());

        // 3. Update the user with blank password (i.e. contains only whitespace)
        logger.debug("Updating user with blank pass...");

        UserDto userBlankPassUpdate = new UserDto();
        userBlankPassUpdate.setUsername(testUserName);
        userBlankPassUpdate.setPassword(null);

        userService.save(userBlankPassUpdate);

        updatedEntity = userService.findByUsername(testUserName);
        assertThat(updatedEntity).as("Unable to locate updated user " + testUserName + ". Update operation failed!").isNotNull();

        logger.debug("Found stored updated with empty pass user\n\t{}\n", updatedEntity);

        assertThat(updatedEntity.getPassword()).as("User password has changed!").isEqualTo(addedEntity.getPassword());
    }

    @Test
    public void testUpdateUserAccountDetails()
            throws Exception
    {
        String testUserName = "test-update-user-account";

        UserDto userAdd = new UserDto();
        userAdd.setEnabled(true);
        userAdd.setUsername(testUserName);
        userAdd.setPassword("test-password");
        userAdd.setSecurityTokenKey("before");

        userService.save(userAdd);

        User addedEntity = userService.findByUsername(testUserName);
        assertThat(addedEntity).as("Unable to locate user " + testUserName + ". Save operation failed!").isNotNull();

        logger.debug("Found stored user\n\t{}\n", addedEntity);
        logger.debug("Updating user...");

        UserDto userUpdate = new UserDto();
        userUpdate.setUsername(testUserName);
        userUpdate.setPassword("another-password");
        userUpdate.setSecurityTokenKey("after");
        userUpdate.setEnabled(false);
        userUpdate.setRoleNames(new HashSet<>(Arrays.asList("a", "b")));

        userService.updateAccountDetailsByUsername(userUpdate);

        User updatedEntity = userService.findByUsername(testUserName);
        assertThat(updatedEntity).as("Unable to locate updated user " + testUserName + ". Update operation failed!").isNotNull();

        logger.debug("Updated user found: \n\t{}\n", updatedEntity);

        assertThat(updatedEntity.getPassword())
                .as("User password should have been encrypted!")
                .isNotEqualTo(addedEntity.getPassword());
        assertThat(updatedEntity.getPassword()).as("User password was updated to null").isNotNull();
        assertThat(updatedEntity.isEnabled()).isTrue();
        assertThat(updatedEntity.getRoles()).as("Expected no user roles to have been updated!").isEmpty();
        assertThat(updatedEntity.getSecurityTokenKey()).isEqualTo("after");
    }

    @Test
    public void testThatUserNameIsUnique()
    {
        assertThat(userService.getUsers().getUsers().stream()
                              .filter(u -> "admin".equals(u.getUsername()))
                              .count())
                .isEqualTo(1);

        UserDto user = new UserDto();
        user.setUsername("admin");

        userService.save(user);

        assertThat(userService.getUsers().getUsers().stream()
                              .filter(u -> "admin".equals(u.getUsername()))
                              .count())
                .isEqualTo(1);
    }

    @Test
    public void testPrivilegesProcessingForAccessModel()
    {
        String testUserName = "test-user";

        UserDto userAdd = new UserDto();
        userAdd.setEnabled(true);
        userAdd.setUsername(testUserName);
        userAdd.setPassword("test-password");
        userAdd.setSecurityTokenKey("before");

        userService.save(userAdd);

        // Load the user
        User user = userService.findByUsername("test-user");

        assertThat(user).as("Unable to find user by name test-user").isNotNull();
    }

    @Test
    public void testDeleteUser()
            throws Exception
    {
        String testUserName = "test-delete-user";

        UserDto userAdd = new UserDto();
        userAdd.setEnabled(true);
        userAdd.setUsername(testUserName);
        userAdd.setPassword("test-password");
        userAdd.setSecurityTokenKey("before");

        userService.save(userAdd);

        User addedEntity = userService.findByUsername(testUserName);
        assertThat(addedEntity).as("Unable to locate user " + testUserName + ". Delete operation failed!").isNotNull();

        logger.debug("Found stored user\n\t{}\n", addedEntity);
        logger.debug("Deleting user...");

        userService.deleteByUsername(testUserName);

        User deletedEntity = userService.findByUsername(testUserName);
        assertThat(deletedEntity)
                .as("User " + testUserName + " is still present in the database. Delete operation failed!")
                .isNull();
    }

}
