package com.veadan.folib.authentication.registry;

import static org.assertj.core.api.Assertions.assertThat;

import com.veadan.folib.authentication.api.password.PasswordAuthenticationProvider;
import com.veadan.folib.config.hazelcast.HazelcastConfiguration;
import com.veadan.folib.config.hazelcast.HazelcastInstanceId;
import com.veadan.folib.authentication.TestConfig;

import javax.inject.Inject;
import java.util.Collection;

import com.google.common.collect.Lists;
import org.junit.jupiter.api.Test;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Import;
import org.springframework.context.annotation.Primary;
import org.springframework.context.annotation.Profile;
import org.springframework.security.authentication.AuthenticationProvider;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.TestPropertySource;

/**
 * @author Przemyslaw Fusik
 */
@SpringBootTest
@ActiveProfiles({ "test", "AuthenticationProvidersRegistryTestConfig" })
@TestPropertySource(properties = { "folib.config.file.authentication.providers=classpath:aprt-folib-authentication-providers.xml",
                                   "folib.authentication.providers.yaml=classpath:/etc/conf/aprt-folib-authentication-providers.yaml" })
@ContextConfiguration(classes = TestConfig.class)
public class AuthenticationProvidersRegistryTest
{

    @Inject
    AuthenticationProvidersRegistry authenticationProvidersRegistry;

    @Test
    public void registryShouldNotBeNull()
    {
        assertThat(getAuthenticationProviderList()).isNotNull();
    }

    private Collection<AuthenticationProvider> getAuthenticationProviderList()
    {
        return authenticationProvidersRegistry.getAuthenticationProviderMap().values();
    }

    @Test
    public void registryShouldContainStrongboxBuiltinAuthenticationProvider()
    {
        assertThat(Lists.newArrayList(getAuthenticationProviderList()))
                .anyMatch(x -> x.getClass().getName().equals(PasswordAuthenticationProvider.class.getName()));
    }

    @Test
    public void registryShouldContainEmptyAuthenticationProvider()
    {
        assertThat(Lists.newArrayList(getAuthenticationProviderList()))
                .anyMatch(x -> x.getClass().getName().equals("EmptyAuthenticationProvider"));
    }
    
    @Profile("AuthenticationProvidersRegistryTestConfig")
    @Import(HazelcastConfiguration.class)
    @Configuration
    public static class AuthenticationProvidersRegistryTestConfig
    {

        @Primary
        @Bean
        public HazelcastInstanceId hazelcastInstanceIdAcctit()
        {
            return new HazelcastInstanceId("AuthenticationProvidersRegistryTest-hazelcast-instance");
        }

    }


}
