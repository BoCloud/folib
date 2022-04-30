package com.veadan.folib.services.support;

import com.veadan.folib.configuration.ConfigurationManager;
import com.veadan.folib.event.repository.RepositoryEvent;
import com.veadan.folib.event.repository.RepositoryEventTypeEnum;
import com.veadan.folib.services.TrustStoreService;
import com.veadan.folib.storage.repository.Repository;
import com.veadan.folib.storage.repository.RepositoryData;
import com.veadan.folib.storage.repository.remote.RemoteRepository;

import javax.inject.Inject;
import java.io.IOException;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.event.EventListener;
import org.springframework.stereotype.Component;

/**
 * @author Przemyslaw Fusik
 */
@Component
public class RepositoryCreatedEventListener
{

    private static final Logger logger = LoggerFactory.getLogger(RepositoryCreatedEventListener.class);


    @Inject
    private ConfigurationManager configurationManager;

    @Inject
    private TrustStoreService trustStoreService;

    @EventListener
    public void handle(RepositoryEvent event)
    {
        if (event.getType() != RepositoryEventTypeEnum.EVENT_REPOSITORY_CREATED.getType())
        {
            return;
        }

        Repository repository = configurationManager.getConfiguration().getStorage(event.getStorageId()).getRepository(
                event.getRepositoryId());

        if (((RepositoryData)repository).getRemoteRepository() != null)
        {
            initializeRemoteRepository(repository.getRemoteRepository());
        }
    }

    private void initializeRemoteRepository(RemoteRepository remoteRepository)
    {
        if (remoteRepository.isAutoImportRemoteSSLCertificate())
        {
            try
            {
                trustStoreService.addSslCertificatesToTrustStore(remoteRepository.getUrl());
            }
            catch (IOException | TrustStoreCertificateOperationException e)
            {
                logger.error("Could not import remote SSL certificate to trust store", e);
            }
        }
    }


}
