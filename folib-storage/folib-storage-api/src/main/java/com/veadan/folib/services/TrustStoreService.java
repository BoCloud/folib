package com.veadan.folib.services;

import com.veadan.folib.services.support.TrustStoreCertificateOperationException;

import java.io.IOException;

/**
 * @author Przemyslaw Fusik
 */
public interface TrustStoreService
{

    void addSslCertificatesToTrustStore(String host)
            throws IOException, TrustStoreCertificateOperationException;

}
