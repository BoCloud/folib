package com.veadan.folib.authentication;

import static org.junit.jupiter.api.Assertions.assertThrows;

import javax.inject.Inject;

import com.veadan.folib.authentication.api.jwt.JwtAuthentication;
import com.veadan.folib.config.hazelcast.HazelcastConfiguration;
import com.veadan.folib.config.hazelcast.HazelcastInstanceId;
import com.veadan.folib.domain.UserEntity;
import com.veadan.folib.domain.SecurityRoleEntity;
import com.veadan.folib.users.dto.UserDto;
import com.veadan.folib.users.security.JwtAuthenticationClaimsProvider;
import com.veadan.folib.users.security.JwtClaimsProvider;
import com.veadan.folib.users.security.SecurityTokenProvider;
import com.veadan.folib.users.service.UserService;
import com.veadan.folib.users.service.impl.EncodedPasswordUser;
import com.veadan.folib.users.service.impl.DatabaseUserService;
import com.veadan.folib.users.service.impl.YamlUserService.Yaml;
import com.veadan.folib.users.userdetails.SpringSecurityUser;
import org.jose4j.lang.JoseException;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.function.Executable;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Import;
import org.springframework.context.annotation.Primary;
import org.springframework.context.annotation.Profile;
import org.springframework.security.authentication.AuthenticationManager;
import org.springframework.security.authentication.BadCredentialsException;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.userdetails.UserDetailsService;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.TestPropertySource;

@SpringBootTest
@ActiveProfiles({ "test", "JwtAuthenticationProviderTestConfig" })
@TestPropertySource(properties = { "folib.config.file.authentication.providers=classpath:japt-folib-authentication-providers.xml",
                                   "folib.authentication.providers.yaml=classpath:/etc/conf/japt-folib-authentication-providers.yaml",
                                   "users.external.cache.seconds=1",
                                   "folib.users.config.yaml=classpath:/etc/conf/japt-folib-security-users.yaml" })
@ContextConfiguration(classes = TestConfig.class)
public class JwtAuthenticationProviderTest
{

    private static final String TEST_USER = "test-user";

    @Inject
    private AuthenticationManager authenticationManager;

    @Inject
    private SecurityTokenProvider securityTokenProvider;

    @Inject
    @JwtAuthenticationClaimsProvider.JwtAuthentication
    private JwtClaimsProvider jwtClaimsProvider;

    @Inject
    private UserDetailsService userDetailsService;

    @Inject
    @Yaml
    private UserService userService;

    @Inject
    private DatabaseUserService databaseUserService;
    
    @Inject
    private PasswordEncoder passwordEncoder;

    @Test
    public void testUserHash()
        throws Exception
    {
        SpringSecurityUser userDetails = (SpringSecurityUser) userDetailsService.loadUserByUsername(TEST_USER);
        String token = securityTokenProvider.getToken(TEST_USER, jwtClaimsProvider.getClaims(userDetails), 3600, null);
        JwtAuthentication authentication = new JwtAuthentication(TEST_USER, token);

        //Authentication should pass with valid token
        authenticationManager.authenticate(authentication);

        //Change user password
        UserDto user = new UserDto();
        user.setUsername(TEST_USER);
        user.setPassword("new_password");
        userService.updateAccountDetailsByUsername(new EncodedPasswordUser(user, passwordEncoder));
        
        databaseUserService.expireUser(TEST_USER, false);
        
        //Authentication should fail by token hash
        assertThrows(BadCredentialsException.class, new Authenticate(authentication)::execute);
        
        //Authentication should pass with valid token
        authenticationManager.authenticate(authentication = getAuthentication(TEST_USER));
        
        //Change roles
        UserEntity userEntity = databaseUserService.findByUsername(TEST_USER);
        userEntity.getRoles().add(new SecurityRoleEntity("LOGS_MANAGER"));
        userService.save(userEntity);
        
        databaseUserService.expireUser(TEST_USER, false);
        
        //Authentication should fail by token hash
        assertThrows(BadCredentialsException.class, new Authenticate(authentication)::execute);
        
        //Authentication should pass with valid token
        authenticationManager.authenticate(getAuthentication(TEST_USER));
    }

    protected JwtAuthentication getAuthentication(String username)
        throws JoseException
    {
        SpringSecurityUser  userDetails = (SpringSecurityUser) userDetailsService.loadUserByUsername(username);
        String token = securityTokenProvider.getToken(TEST_USER, jwtClaimsProvider.getClaims(userDetails), 3600, null);
        return  new JwtAuthentication(TEST_USER, token);
    }

    @Profile("JwtAuthenticationProviderTestConfig")
    @Import(HazelcastConfiguration.class)
    @Configuration
    public static class JwtAuthenticationProviderTestConfig
    {

        @Primary
        @Bean
        public HazelcastInstanceId hazelcastInstanceIdAcctit()
        {
            return new HazelcastInstanceId("JwtAuthenticationProviderTest-hazelcast-instance");
        }

    }

    private class Authenticate implements Executable {
        
        private final Authentication authentication;

        public Authenticate(Authentication authentication)
        {
            this.authentication = authentication;
        }

        @Override
        public void execute()
            throws Throwable
        {
            authenticationManager.authenticate(authentication);
        }
        
    }
}
