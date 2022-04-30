package com.veadan.folib.authentication.api.impl.ldap;

import com.veadan.folib.authentication.support.AuthenticationContextInitializer;
import com.veadan.folib.config.UsersConfig;
import com.veadan.folib.users.domain.SystemRole;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import javax.inject.Inject;
import java.io.InputStream;
import java.lang.reflect.UndeclaredThrowableException;
import java.util.List;
import java.util.Properties;

import com.veadan.folib.config.LdapServerTestConfig;
import com.veadan.folib.configuration.StrongboxSecurityConfig;

import org.junit.jupiter.api.Test;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.core.env.PropertiesPropertySource;
import org.springframework.core.env.PropertySource;
import org.springframework.ldap.core.ContextSource;
import org.springframework.ldap.core.DirContextAdapter;
import org.springframework.ldap.core.LdapTemplate;
import org.springframework.security.core.authority.SimpleGrantedAuthority;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.security.ldap.userdetails.LdapUserDetails;
import org.springframework.security.ldap.userdetails.LdapUserDetailsImpl;
import org.springframework.security.ldap.userdetails.LdapUserDetailsService;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.ContextHierarchy;
import static org.assertj.core.api.Assertions.assertThat;

/**
 * @author Przemyslaw Fusik
 * @author sbespalov
 */
@SpringBootTest(properties = {"tests.unboundid.importLdifs=/ldap/00-folib-base.ldif,/ldap/10-issue-1840.ldif"})
@ContextHierarchy({ @ContextConfiguration(classes = { UsersConfig.class,
                                                      StrongboxSecurityConfig.class,
                                                      LdapServerTestConfig.class }),
                    @ContextConfiguration(initializers = LdapAuthenticationProviderTest.TestContextInitializer.class,
                                          locations = "classpath:/com/veadan/folib/authentication/external/ldap/folib-authentication-providers.xml") })
@ActiveProfiles(profiles = "test")
public class LdapAuthenticationProviderTest
{

    @Inject
    private ContextSource contextSource;

    @Inject
    private LdapUserDetailsService ldapUserDetailsService;

    @Inject
    private PasswordEncoder passwordEncoder;

    @Test
    public void embeddedLdapServerCreationContainsExpectedContextSourceAndData()
            throws Exception
    {
        LdapTemplate template = new LdapTemplate(contextSource);
        Object ldapObject = template.lookup("uid=przemyslaw.fusik,ou=Users");

        assertThat(ldapObject).isNotNull();
        assertThat(ldapObject).isInstanceOf(DirContextAdapter.class);

        DirContextAdapter dirContextAdapter = (DirContextAdapter) ldapObject;

        assertThat(dirContextAdapter.getDn().toString()).isEqualTo("uid=przemyslaw.fusik,ou=Users");
        assertThat(dirContextAdapter.getNameInNamespace()).isEqualTo("uid=przemyslaw.fusik,ou=Users,dc=carlspring,dc=com");
    }

    @Test
    public void embeddedLdapServerRegistersExpectedAuthenticationProvider()
    {
        UserDetails ldapUser = ldapUserDetailsService.loadUserByUsername("przemyslaw.fusik");

        assertThat(ldapUser).isInstanceOf(LdapUserDetailsImpl.class);

        LdapUserDetails ldapUserDetails = (LdapUserDetailsImpl) ldapUser;

        assertThat(ldapUserDetails.getDn()).isEqualTo("uid=przemyslaw.fusik,ou=Users,dc=carlspring,dc=com");
        assertThat(ldapUserDetails.getPassword()).isEqualTo(
                "{SHA-256}{mujKRdqeWWYAWhczNwVnBl6L6dHNwWO5eIGZ/G7pnBg=}bb63813f5b6f64ae306ebbbb23dcbb1c6f49eb9b989fc466b1b1a24a011bb2ce");
        assertThat(ldapUserDetails.getUsername()).isEqualTo("przemyslaw.fusik");
        assertThat(((List<SimpleGrantedAuthority>) ldapUser.getAuthorities()))
                .contains(
                        new SimpleGrantedAuthority(SystemRole.REPOSITORY_MANAGER.name()),
                        new SimpleGrantedAuthority("USER_ROLE")
                );
        assertThat(passwordEncoder.matches("password", ldapUserDetails.getPassword())).isEqualTo(true);
    }

    @Test
    public void base64EncodedPasswordAfterAlgorithmShouldFail()
    {
        UserDetails ldapUser = ldapUserDetailsService.loadUserByUsername("issue1840-type-1");

        assertThat(ldapUser).isInstanceOf(LdapUserDetailsImpl.class);

        LdapUserDetails ldapUserDetails = (LdapUserDetailsImpl) ldapUser;

        assertThat(passwordEncoder.matches("password", ldapUserDetails.getPassword())).isEqualTo(false);
    }

    @Test
    public void base64EncodedPasswordWithAlgorithmShouldFail()
    {
        UserDetails ldapUser = ldapUserDetailsService.loadUserByUsername("issue1840-type-2");

        assertThat(ldapUser).isInstanceOf(LdapUserDetailsImpl.class);

        LdapUserDetails ldapUserDetails = (LdapUserDetailsImpl) ldapUser;

        Exception thrown = assertThrows(IllegalArgumentException.class,
                                        () -> passwordEncoder.matches("password", ldapUserDetails.getPassword()),
                                        "There is no PasswordEncoder mapped for the id \"null\"");

        assertTrue(thrown.getMessage().contains("There is no PasswordEncoder mapped for the id \"null\""));
    }

    public static class TestContextInitializer
            extends AuthenticationContextInitializer
    {

        public TestContextInitializer()
        {
            super(loadPropertySource());
        }

        private static PropertySource<?> loadPropertySource()
        {
            Properties properties = new Properties();
            try (InputStream is = LdapAuthenticationProviderTest.class.getResourceAsStream("lapt.properties"))
            {
                properties.load(is);
            }
            catch (Exception ex)
            {
                throw new UndeclaredThrowableException(ex);
            }

            return new PropertiesPropertySource(FOLIB_AUTHENTICATION_PROVIDERS, properties);
        }
    }

}
