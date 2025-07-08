package com.folib.services.support;

import com.folib.configuration.ConfigurationManager;
import com.folib.event.repository.RepositoryEvent;
import com.folib.event.repository.RepositoryEventTypeEnum;
import com.folib.services.TrustStoreService;
import com.folib.storage.repository.Repository;
import com.folib.storage.repository.RepositoryData;
import com.folib.storage.repository.remote.RemoteRepository;
import com.folib.storage.repository.remote.heartbeat.RemoteRepositoriesHeartbeatMonitorInitiator;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.event.EventListener;
import org.springframework.stereotype.Component;

import jakarta.inject.Inject;
import java.io.IOException;

/**
 * @author veadan
 */
@Component
public class RepositoryCreatedEventListener {

    private static final Logger logger = LoggerFactory.getLogger(RepositoryCreatedEventListener.class);


    @Inject
    private ConfigurationManager configurationManager;

    @Inject
    private TrustStoreService trustStoreService;

    @Inject
    private RemoteRepositoriesHeartbeatMonitorInitiator remoteRepositoriesHeartbeatMonitorInitiator;

    @EventListener
    public void handle(RepositoryEvent event) {
        if (event.getType() != RepositoryEventTypeEnum.EVENT_REPOSITORY_CREATED.getType()) {
            return;
        }

        Repository repository = configurationManager.getConfiguration().getStorage(event.getStorageId()).getRepository(
                event.getRepositoryId());

        if (((RepositoryData) repository).getRemoteRepository() != null) {
            initializeRemoteRepository(repository, repository.getRemoteRepository());
        }
    }

    private void initializeRemoteRepository(Repository repository, RemoteRepository remoteRepository) {
        remoteRepositoriesHeartbeatMonitorInitiator.scheduleRemoteRepositoryMonitoring(remoteRepositoriesHeartbeatMonitorInitiator.getDefaultRemoteRepositoriesHeartbeatIntervalSeconds(), repository.getStorageIdAndRepositoryId());
        if (remoteRepository.isAutoImportRemoteSSLCertificate()) {
            try {
                trustStoreService.addSslCertificatesToTrustStore(remoteRepository.getUrl());
            } catch (IOException | TrustStoreCertificateOperationException e) {
                logger.error("Could not import remote SSL certificate to trust store", e);
            }
        }
    }
}
