package com.veadan.folib.config;

import javax.annotation.PreDestroy;
import java.io.InputStream;
import java.util.List;
import java.util.stream.Collectors;

import com.unboundid.ldap.listener.InMemoryDirectoryServer;
import com.unboundid.ldap.listener.InMemoryDirectoryServerConfig;
import com.unboundid.ldap.listener.InMemoryListenerConfig;
import com.unboundid.ldap.sdk.DN;
import com.unboundid.ldap.sdk.Entry;
import com.unboundid.ldap.sdk.LDAPException;
import com.unboundid.ldif.LDIFReader;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.core.io.ClassPathResource;
import org.springframework.core.io.Resource;
import org.springframework.util.StringUtils;

/**
 * This class is based on {@link org.springframework.boot.autoconfigure.ldap.embedded.EmbeddedLdapAutoConfiguration}
 * and {@link org.springframework.security.ldap.server.UnboundIdContainer}.
 *
 * The reason for creating a new class is to allow for UnboundID server customizations which is not available through
 * configuration files / bean definitions. We need the customizations in order to unify the configuration in the UI,
 * integration tests and manual tests through the browser and OpenLDAP.
 */
@Configuration
public class LdapServerTestConfig
{

    private static final Logger logger = LoggerFactory.getLogger(LdapServerTestConfig.class);

    private InMemoryDirectoryServer server;

    @Value("${tests.unboundid.baseDn:dc=carlspring,dc=com}")
    private String baseDn;

    @Value("${tests.unboundid.managerDn:cn=admin,dc=carlspring,dc=com}")
    private String managerDn;

    @Value("${tests.unbounbid.masterPassword:password}")
    private String managerPassword;

    @Value("${tests.unboundid.port:53389}")
    private int port;

    /**
     * You can use `@SpringBootTest(properties = {"tests.unboundid.importLdifs=folib-base.ldif,folib-test.ldif"})`
     * to overwrite this parameter per test.
     */
    @Value("#{'${tests.unboundid.importLdifs:/ldap/00-folib-base.ldif}'.split(',')}")
    private String[] ldifCollection;

    @Bean
    public InMemoryDirectoryServer ldapTestServer()
    {
        if (server != null)
        {
            return server;
        }

        try
        {
            logger.info("UnboundID: Configuring server with base dn [{}]", baseDn);
            InMemoryDirectoryServerConfig config = new InMemoryDirectoryServerConfig(baseDn);
            config.setListenerConfigs(InMemoryListenerConfig.createLDAPConfig("LDAP", this.port));
            config.setEnforceSingleStructuralObjectClass(false);
            config.setEnforceAttributeSyntaxCompliance(true);
            // This is necessary to allow for `changeType: add` in LDIF files.
            config.setSchema(null);

            configureCredentials(config);

            DN dn = new DN(baseDn);
            Entry entry = new Entry(dn);
            entry.addAttribute("objectClass", "top", "domain", "extensibleObject");
            entry.addAttribute("dc", dn.getRDN().getAttributeValues()[0]);

            InMemoryDirectoryServer directoryServer = new InMemoryDirectoryServer(config);
            directoryServer.add(entry);
            importLdifCollection(directoryServer);
            directoryServer.startListening();
            server = directoryServer;

            logger.info("UnboundID: Now accepts connections on port {}", port);
        }
        catch (LDAPException ex)
        {
            throw new RuntimeException("Server startup failed", ex);
        }

        return server;
    }

    private void configureCredentials(InMemoryDirectoryServerConfig config)
            throws LDAPException
    {
        // keeping this for backwards compatibility with Spring's default.
        config.addAdditionalBindCredentials("uid=admin,ou=system", "secret");
        config.addAdditionalBindCredentials("uid=admin,ou=system," + baseDn, "secret");

        // Always add manager DN / password
        config.addAdditionalBindCredentials(managerDn, managerPassword);

        logger.info("UnboundID: Available bind credentials {}", config.getAdditionalBindCredentials()
                                                                      .keySet()
                                                                      .stream()
                                                                      .map(DN::getRDNString)
                                                                      .map((s) -> "dn: " + s)
                                                                      .collect(Collectors.toList()));

    }

    private void importLdifCollection(InMemoryDirectoryServer directoryServer)
    {
        for (String ldif : ldifCollection)
        {
            if (StringUtils.hasText(ldif))
            {
                Resource resource = new ClassPathResource(ldif);
                try
                {
                    if (resource.exists())
                    {
                        logger.info("UnboundID: Importing file [classpath:{}]", ldif);
                        try (InputStream inputStream = resource.getInputStream())
                        {
                            directoryServer.applyChangesFromLDIF(new LDIFReader(inputStream));
                        }
                    }
                    else
                    {
                        logger.warn("Unable to locate {} file; skipping import!", ldif);
                    }
                }
                catch (Exception ex)
                {
                    throw new IllegalStateException("Unable to load LDIF " + ldif, ex);
                }
            }
        }
    }

    public List<String> getManagerBindings()
    {
        return server.getConfig()
                     .getAdditionalBindCredentials()
                     .keySet()
                     .stream()
                     .map(DN::getRDNString)
                     .collect(Collectors.toList());
    }

    @PreDestroy
    public void close()
    {
        if (server != null)
        {
            logger.info("UnboundID: Shutting down.");
            server.shutDown(true);
            server = null;
        }
    }

}
