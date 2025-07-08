package com.folib.services;

import com.folib.services.support.TrustStoreCertificateOperationException;

import java.io.IOException;

/**
 * @author veadan
 */
public interface TrustStoreService
{

    void addSslCertificatesToTrustStore(String host)
            throws IOException, TrustStoreCertificateOperationException;

}
