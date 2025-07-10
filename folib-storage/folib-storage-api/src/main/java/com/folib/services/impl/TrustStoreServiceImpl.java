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
package com.folib.services.impl;

import com.folib.services.support.TrustStoreCertificateOperationException;
import com.folib.resource.ConfigurationResourceResolver;
import com.folib.security.certificates.KeyStoreManager;
import com.folib.services.TrustStoreService;

import javax.annotation.PostConstruct;
import jakarta.inject.Inject;
import java.io.IOException;
import java.net.InetAddress;
import java.net.URL;
import java.nio.file.Paths;
import java.security.KeyManagementException;
import java.security.KeyStoreException;
import java.security.NoSuchAlgorithmException;
import java.security.cert.CertificateException;

import org.springframework.core.io.Resource;
import org.springframework.stereotype.Service;

/**
 * @author veadan
 */
@Service
public class TrustStoreServiceImpl
        implements TrustStoreService
{

    private static final String PASSWORD = "password";

    private Resource trustStore;

    @Inject
    private KeyStoreManager keyStoreManager;

    @Inject
    private ConfigurationResourceResolver configurationResourceResolver;


    @PostConstruct
    public void init()
    throws IOException
    {
        trustStore = getTrustStoreResource();
    }

    @Override
    public void addSslCertificatesToTrustStore(String host)
            throws IOException, TrustStoreCertificateOperationException
    {
        final URL url = new URL(host);
        final String urlHost = url.getHost();
        final int urlPort = url.getPort() != -1 ? url.getPort() : url.getDefaultPort();

        try
        {
            keyStoreManager.addCertificates(Paths.get(trustStore.getURI()),
                                            PASSWORD.toCharArray(),
                                            InetAddress.getByName(urlHost),
                                            urlPort);
        }
        catch (IOException | CertificateException | NoSuchAlgorithmException | KeyStoreException | KeyManagementException ex)
        {
            throw new TrustStoreCertificateOperationException(ex);
        }
    }

    private Resource getTrustStoreResource()
    {
        return configurationResourceResolver.getConfigurationResource("folib.truststore.jks",
                                                                      "etc/ssl/truststore.jks");
    }

}
