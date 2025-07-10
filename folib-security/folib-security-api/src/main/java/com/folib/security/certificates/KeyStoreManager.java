/*
 * Folib - [新一代AI制品仓库]
 * Copyright (C) 2025 bocloud.com.cn <folib@beyondcent.com>
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * 本程序是自由软件：您可依据GNU通用公共许可证（GPL-3.0+）条款重新发布和修改，
 * 但禁止任何形式的商业售卖行为（包括但不限于：直接销售、捆绑销售、云服务商用）。
 *
 * This program is distributed WITHOUT ANY WARRANTY.
 * Commercial sale of this software is expressly prohibited.
 *
 * For license details, see: https://www.gnu.org/licenses/gpl-3.0.html
 * 商业授权咨询请联系：folib@beyondcent.com
 */
package com.folib.security.certificates;

import javax.annotation.PostConstruct;
import javax.inject.Inject;
import javax.net.ssl.*;
import java.io.*;
import java.net.*;
import java.nio.file.Files;
import java.nio.file.Path;
import java.security.KeyManagementException;
import java.security.KeyStore;
import java.security.KeyStoreException;
import java.security.NoSuchAlgorithmException;
import java.security.cert.Certificate;
import java.security.cert.CertificateException;
import java.security.cert.X509Certificate;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.Map;

import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

@Component
public class KeyStoreManager
{

    private static final Logger logger = LoggerFactory.getLogger(KeyStoreManager.class);

    @Inject
    private ProxyAuthenticator proxyAuthenticator;


    public KeyStoreManager()
    {
    }

    @PostConstruct
    public void init()
    {
        Authenticator.setDefault(proxyAuthenticator);
    }

    private KeyStore load(Path path,
                          char[] password)
            throws KeyStoreException,
                   IOException,
                   CertificateException,
                   NoSuchAlgorithmException
    {
        KeyStore keyStore = KeyStore.getInstance(KeyStore.getDefaultType());
        if (path == null)
        {
            keyStore.load(null, password);
            return keyStore;
        }

        try (InputStream is = new BufferedInputStream(Files.newInputStream(path)))
        {
            keyStore.load(is, password);
            return keyStore;
        }
    }

    private KeyStore store(Path path,
                           char[] password,
                           KeyStore keyStore)
            throws IOException,
                   CertificateException,
                   NoSuchAlgorithmException,
                   KeyStoreException
    {
        try (OutputStream os = new BufferedOutputStream(Files.newOutputStream(path)))
        {
            keyStore.store(os, password);
            return keyStore;
        }
    }

    public KeyStore createNew(Path path,
                              char[] password)
            throws KeyStoreException,
                   CertificateException,
                   NoSuchAlgorithmException,
                   IOException
    {
        return store(path, password, load(null, password));
    }

    public KeyStore changePassword(Path path,
                                   char[] oldPassword,
                                   char[] newPassword)
            throws KeyStoreException,
                   IOException,
                   CertificateException,
                   NoSuchAlgorithmException
    {
        return store(path, newPassword, load(path, oldPassword));
    }

    public Map<String, Certificate> listCertificates(Path path,
                                                     char[] password)
            throws KeyStoreException,
                   IOException,
                   CertificateException,
                   NoSuchAlgorithmException
    {
        KeyStore keyStore = load(path, password);
        Map<String, Certificate> certificates = new HashMap<>();
        Enumeration<String> aliases = keyStore.aliases();

        while (aliases.hasMoreElements())
        {
            String alias = aliases.nextElement();
            certificates.put(alias, keyStore.getCertificate(alias));
        }

        return certificates;
    }

    public KeyStore removeCertificates(Path path,
                                       char[] password,
                                       InetAddress host,
                                       int port)
            throws KeyStoreException,
                   IOException,
                   CertificateException,
                   NoSuchAlgorithmException
    {
        KeyStore keyStore = load(path, password);
        String prefix = host.getCanonicalHostName() + ":" + port;

        Enumeration<String> aliases = keyStore.aliases();
        while (aliases.hasMoreElements())
        {
            String alias = aliases.nextElement();
            if (StringUtils.startsWithIgnoreCase(alias, prefix))
            {
                keyStore.deleteEntry(alias);
            }
        }

        return store(path, password, keyStore);
    }

    public KeyStore addCertificates(Path path,
                                    char[] password,
                                    InetAddress host,
                                    int port)
            throws KeyStoreException,
                   IOException,
                   CertificateException,
                   NoSuchAlgorithmException,
                   KeyManagementException
    {
        KeyStore keyStore = load(path, password);
        String prefix = host.getCanonicalHostName() + ":" + port;
        X509Certificate[] chain = remoteCertificateChain(host, port);

        for (X509Certificate cert : chain)
        {
            keyStore.setCertificateEntry(prefix + "_" + cert.getSubjectDN().getName(), cert);
        }

        return store(path, password, keyStore);
    }

    public KeyStore addHttpsCertificates(Path path,
                                         char[] password,
                                         Proxy httpProxy,
                                         PasswordAuthentication credentials,
                                         String host,
                                         int port)
            throws CertificateException,
                   NoSuchAlgorithmException,
                   KeyStoreException,
                   IOException,
                   KeyManagementException
    {
        KeyStore keyStore = load(path, password);
        String prefix = host + ":" + port;
        ChainCaptureTrustManager tm = new ChainCaptureTrustManager();
        SSLContext ctx = SSLContext.getInstance("TLS");
        ctx.init(null, new TrustManager[]{ tm }, null);

        URL url = new URL("https", host, port, "/");
        HttpsURLConnection conn = (HttpsURLConnection) (httpProxy != null ?
                                                        url.openConnection(httpProxy) :
                                                        url.openConnection());
        conn.setSSLSocketFactory(ctx.getSocketFactory());

        if (credentials != null)
        {
            proxyAuthenticator.getCredentials().set(credentials);
        }

        try
        {
            conn.connect();
            for (X509Certificate cert : tm.chain)
            {
                keyStore.setCertificateEntry(prefix + "_" + cert.getSubjectDN().getName(), cert);
            }
            return store(path, password, keyStore);
        }
        finally
        {
            proxyAuthenticator.getCredentials().remove();
            conn.disconnect();
        }

    }

    public KeyStore addSslCertificates(Path path,
                                       char[] password,
                                       Proxy socksProxy,
                                       PasswordAuthentication credentials,
                                       String host,
                                       int port)
            throws KeyStoreException,
                   IOException,
                   CertificateException,
                   NoSuchAlgorithmException,
                   KeyManagementException
    {
        KeyStore keyStore = load(path, password);
        String prefix = host + ":" + port;
        X509Certificate[] chain = remoteCertificateChain(socksProxy, credentials, host, port);

        for (X509Certificate cert : chain)
        {
            keyStore.setCertificateEntry(prefix + "_" + cert.getSubjectDN().getName(), cert);
        }

        return store(path, password, keyStore);
    }

    private X509Certificate[] remoteCertificateChain(InetAddress address,
                                                     int port)
            throws NoSuchAlgorithmException,
                   IOException,
                   KeyManagementException
    {
        ChainCaptureTrustManager tm = new ChainCaptureTrustManager();
        SSLContext ctx = SSLContext.getInstance("TLS");
        ctx.init(null, new TrustManager[]{ tm }, null);
        SSLSocket socket = (SSLSocket) ctx.getSocketFactory().createSocket(address, port);

        try
        {
            socket.startHandshake();
            socket.close();
        }
        catch (SSLException ignore) // non trusted certificates should be returned as well
        {
        }

        return tm.chain;
    }

    private X509Certificate[] remoteCertificateChain(Proxy socksProxy,
                                                     PasswordAuthentication credentials,
                                                     String host,
                                                     int port)
            throws NoSuchAlgorithmException,
                   IOException,
                   KeyManagementException
    {
        ChainCaptureTrustManager tm = new ChainCaptureTrustManager();
        SSLContext ctx = SSLContext.getInstance("TLS");
        ctx.init(null, new TrustManager[]{ tm }, null);

        if (credentials != null)
        {
            proxyAuthenticator.getCredentials().set(credentials);
        }

        try (Socket proxySocket = socksProxy != null ? new Socket(socksProxy) : null)
        {
            if (proxySocket != null)
            {
                proxySocket.connect(new InetSocketAddress(host, port));
            }

            try
            {
                handshake(ctx, proxySocket, host, port);
                return tm.chain;
            }
            finally
            {
                proxyAuthenticator.getCredentials().remove();
            }
        }
    }

    private void handshake(SSLContext ctx,
                           Socket proxySocket,
                           String host,
                           int port)
            throws IOException
    {
        try (SSLSocket socket = (SSLSocket) (proxySocket == null ?
                                             ctx.getSocketFactory().createSocket(host, port) :
                                             ctx.getSocketFactory().createSocket(proxySocket, host, port, true)))
        {
            try
            {
                socket.startHandshake();
            }
            catch (SSLException ignore) // non trusted certificates should be returned as well
            {
            }
        }
    }

    private class ChainCaptureTrustManager
            implements X509TrustManager
    {

        private X509Certificate[] chain;

        @Override
        public X509Certificate[] getAcceptedIssuers()
        {
            return new X509Certificate[0];
        }

        @Override
        public void checkClientTrusted(X509Certificate[] chain,
                                       String authType)
        {
            throw new UnsupportedOperationException();
        }

        @Override
        public void checkServerTrusted(X509Certificate[] chain,
                                       String authType)
        {
            this.chain = chain;
        }
    }
}
