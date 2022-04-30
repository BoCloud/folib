package com.veadan.folib.security.certificates;

import com.veadan.folib.net.ConnectionChecker;
import com.veadan.folib.testing.AssignedPorts;

import javax.inject.Inject;
import java.io.File;
import java.io.IOException;
import java.net.InetAddress;
import java.net.InetSocketAddress;
import java.net.PasswordAuthentication;
import java.net.Proxy;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.security.KeyManagementException;
import java.security.KeyStore;
import java.security.KeyStoreException;
import java.security.NoSuchAlgorithmException;
import java.security.cert.Certificate;
import java.security.cert.CertificateException;
import java.security.cert.X509Certificate;
import java.util.Map;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.context.ContextConfiguration;
import static org.assertj.core.api.Assertions.assertThat;

@SpringBootTest
@ActiveProfiles(profiles = "test")
@ContextConfiguration
public class KeyStoreManagerIntegrationTestIT
{

    @org.springframework.context.annotation.Configuration
    @ComponentScan(basePackages = { "com.veadan.folib.security",
                                    "com.veadan.folib.testing" })
    public static class SpringConfig { }

    @Inject
    private AssignedPorts assignedPorts;

    private static final String PROXY_USERNAME = "testuser";

    private static final String PROXY_PASSWORD = "password";

    private static final String KEYSTORE_PASSWORD = "password";

    private static final String PROXY_HOST = "localhost";

    private static final String SOCKS_HOST = "192.168.100.1";

    private static final int PROXY_SOCKS_PORT = 15035;

    private static int PROXY_HTTP_PORT;

    private final Proxy PROXY_SOCKS = new Proxy(Proxy.Type.SOCKS, new InetSocketAddress(PROXY_HOST, PROXY_SOCKS_PORT));

    private final Proxy PROXY_HTTP = new Proxy(Proxy.Type.HTTP, new InetSocketAddress(PROXY_HOST, PROXY_HTTP_PORT));

    private static final PasswordAuthentication credentials = new PasswordAuthentication(PROXY_USERNAME,
                                                                                         PROXY_PASSWORD.toCharArray());

    public static int LDAPS_PORT;

    private Path f;

    @Inject
    KeyStoreManager keyStoreManager;


    @BeforeEach
    public void init()
            throws IOException,
                   CertificateException,
                   NoSuchAlgorithmException,
                   KeyStoreException
    {
        //noinspection ResultOfMethodCallIgnored
        new File("target/test-resources").mkdirs();
        f = Paths.get("target", "test-resources", "test.jks");

        PROXY_HTTP_PORT = assignedPorts.getPort("port.littleproxy");
        LDAPS_PORT = assignedPorts.getPort("port.unboundid");
    }

    @Test
    public void testWithoutProxy()
            throws IOException,
                   CertificateException,
                   NoSuchAlgorithmException,
                   KeyStoreException,
                   KeyManagementException
    {
        keyStoreManager.createNew(f, KEYSTORE_PASSWORD.toCharArray());
        final KeyStore ks = keyStoreManager.addCertificates(f,
                                                            KEYSTORE_PASSWORD.toCharArray(),
                                                            InetAddress.getLocalHost(),
                                                            LDAPS_PORT);

        assertThat(ks.size()).as("localhost should have three certificates in the chain").isEqualTo(1);

        Map<String, Certificate> certs = keyStoreManager.listCertificates(f, KEYSTORE_PASSWORD.toCharArray());
        for (final Map.Entry<String, Certificate> cert : certs.entrySet())
        {
            System.out.println(cert.getKey() + " : " + ((X509Certificate)cert.getValue()).getSubjectDN());
        }

        final String newPassword = "newpassword";

        keyStoreManager.changePassword(f, KEYSTORE_PASSWORD.toCharArray(), newPassword.toCharArray());
        keyStoreManager.removeCertificates(f, newPassword.toCharArray(), InetAddress.getLocalHost(), LDAPS_PORT);
        certs = keyStoreManager.listCertificates(f, newPassword.toCharArray());

        assertThat(certs).as("Expected empty certs.").isEmpty();
    }

    @Test
    public void testSocksProxy()
            throws IOException,
                   CertificateException,
                   NoSuchAlgorithmException,
                   KeyStoreException,
                   KeyManagementException
    {
        if (!ConnectionChecker.checkServiceAvailability(SOCKS_HOST, PROXY_SOCKS_PORT, 5000))
        {
            System.out.println("WARN: Skipping the testSocks() test, as the proxy server is unreachable.");

            return;
        }

        keyStoreManager.createNew(f, KEYSTORE_PASSWORD.toCharArray());
        final KeyStore ks = keyStoreManager.addSslCertificates(f,
                                                               KEYSTORE_PASSWORD.toCharArray(),
                                                               PROXY_SOCKS,
                                                               credentials,
                                                               "google.com",
                                                               443);

        assertThat(ks.size()).as("localhost should have one certificate in the chain").isEqualTo(1);

        Map<String, Certificate> certs = keyStoreManager.listCertificates(f, KEYSTORE_PASSWORD.toCharArray());
        for (final Map.Entry<String, Certificate> cert : certs.entrySet())
        {
            System.out.println(cert.getKey() + " : " + ((X509Certificate) cert.getValue()).getSubjectDN());
        }

        final String newPassword = "newpassword";

        keyStoreManager.changePassword(f, KEYSTORE_PASSWORD.toCharArray(), newPassword.toCharArray());
        keyStoreManager.removeCertificates(f, newPassword.toCharArray(), InetAddress.getLocalHost(), LDAPS_PORT);
        certs = keyStoreManager.listCertificates(f, newPassword.toCharArray());

        assertThat(certs.isEmpty()).isTrue();
    }

    @Disabled
    @Test
    public void testHttpProxy()
            throws IOException,
                   CertificateException,
                   NoSuchAlgorithmException,
                   KeyStoreException,
                   KeyManagementException
    {
        if (!ConnectionChecker.checkServiceAvailability(PROXY_HOST, PROXY_HTTP_PORT, 5000))
        {
            System.out.println("WARN: Skipping the testHttp() test, as the proxy server is unreachable.");
            return;
        }

        System.out.println("Executing HTTP proxy test...");

        keyStoreManager.createNew(f, KEYSTORE_PASSWORD.toCharArray());
        final KeyStore ks = keyStoreManager.addHttpsCertificates(f,
                                                                 KEYSTORE_PASSWORD.toCharArray(),
                                                                 PROXY_HTTP,
                                                                 credentials,
                                                                 "google.com",
                                                                 443);

        assertThat(ks.size()).as("google.com should have three certificate in the chain").isEqualTo(3);

        Map<String, Certificate> certs = keyStoreManager.listCertificates(f, KEYSTORE_PASSWORD.toCharArray());
        for (final Map.Entry<String, Certificate> cert : certs.entrySet())
        {
            System.out.println(cert.getKey() + " : " + ((X509Certificate) cert.getValue()).getSubjectDN());
        }
    }

}
