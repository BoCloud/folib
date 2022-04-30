package com.veadan.folib.authentication.api.impl.ldap;

import com.veadan.folib.authentication.support.AuthenticationContextInitializer;
import com.veadan.folib.config.LdapServerTestConfig;
import com.veadan.folib.config.UsersConfig;
import com.veadan.folib.configuration.StrongboxSecurityConfig;
import com.veadan.folib.users.domain.SystemRole;

import javax.inject.Inject;
import java.io.InputStream;
import java.lang.reflect.UndeclaredThrowableException;
import java.util.List;
import java.util.Properties;

import org.junit.jupiter.api.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.core.env.PropertiesPropertySource;
import org.springframework.core.env.PropertySource;
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
 * Test Class for checking Base64Encoded LDAP password
 *
 * @author mbharti
 * @date 18/11/20
 */
@SpringBootTest(properties = {"tests.unboundid.importLdifs=/ldap/00-folib-base.ldif,/ldap/10-issue-1840.ldif"})
@ContextHierarchy({ @ContextConfiguration(classes = { UsersConfig.class,
                                                      StrongboxSecurityConfig.class,
                                                      LdapServerTestConfig.class }),
                    @ContextConfiguration(initializers = LdapAuthenticationBase64EncodedTest.TestContextInitializer.class,
                                          locations = "classpath:/com/veadan/folib/authentication/external/ldap/folib-authentication-providers.xml") })
@ActiveProfiles(profiles = "test")
public class LdapAuthenticationBase64EncodedTest
{

    private static final Logger logger = LoggerFactory.getLogger(LdapAuthenticationBase64EncodedTest.class);

    @Inject
    private LdapUserDetailsService ldapUserDetailsService;

    @Inject
    private PasswordEncoder passwordEncoder;

    @Test
    public void base64EncodedPasswordAfterAlgorithmShouldWork()
    {
        UserDetails ldapUser = ldapUserDetailsService.loadUserByUsername("issue1840-type-1");

        assertThat(ldapUser).isInstanceOf(LdapUserDetailsImpl.class);

        LdapUserDetails ldapUserDetails = (LdapUserDetailsImpl) ldapUser;

        assertThat(ldapUserDetails.getDn()).isEqualTo("uid=issue1840-type-1,ou=Users,dc=carlspring,dc=com");
        assertThat(ldapUserDetails.getUsername()).isEqualTo("issue1840-type-1");
        assertThat(ldapUserDetails.getPassword()).isEqualTo("{MD5}5f4dcc3b5aa765d61d8327deb882cf99");
        assertThat(passwordEncoder.matches("password", ldapUserDetails.getPassword())).isEqualTo(true);
        assertThat(((List<SimpleGrantedAuthority>) ldapUser.getAuthorities()))
                .contains(new SimpleGrantedAuthority(SystemRole.REPOSITORY_MANAGER.name()),
                          new SimpleGrantedAuthority("USER_ROLE"));
    }

    @Test
    public void base64EncodedPasswordWithAlgorithmShouldWork()
    {
        UserDetails ldapUser = ldapUserDetailsService.loadUserByUsername("issue1840-type-2");

        assertThat(ldapUser).isInstanceOf(LdapUserDetailsImpl.class);

        LdapUserDetails ldapUserDetails = (LdapUserDetailsImpl) ldapUser;

        assertThat(ldapUserDetails.getDn()).isEqualTo(
                "uid=issue1840-type-2,ou=Users,dc=carlspring,dc=com");
        assertThat(ldapUserDetails.getUsername()).isEqualTo("issue1840-type-2");
        assertThat(ldapUserDetails.getPassword()).isEqualTo(
                "{bcrypt}$2a$10$lpwlxyjvXKzN1ccCrw2PBuZx.eVesWbfmTbsrCboMU.gsNWVcZWMi");
        assertThat(passwordEncoder.matches("password", ldapUserDetails.getPassword())).isEqualTo(true);
        assertThat(((List<SimpleGrantedAuthority>) ldapUser.getAuthorities()))
                .contains(new SimpleGrantedAuthority(SystemRole.REPOSITORY_MANAGER.name()),
                          new SimpleGrantedAuthority("USER_ROLE"));
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
            try (InputStream is = LdapAuthenticationProviderTest.class.getResourceAsStream("labet.properties"))
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
