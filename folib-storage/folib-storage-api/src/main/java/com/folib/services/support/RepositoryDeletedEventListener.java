package com.folib.services.support;

import com.folib.configuration.ConfigurationUtils;
import com.folib.event.repository.RepositoryEvent;
import com.folib.event.repository.RepositoryEventTypeEnum;
import com.folib.storage.repository.remote.heartbeat.RemoteRepositoriesHeartbeatMonitorInitiator;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.event.EventListener;
import org.springframework.stereotype.Component;

import jakarta.inject.Inject;

/**
 * @author veadan
 */
@Component
public class RepositoryDeletedEventListener {

    private static final Logger logger = LoggerFactory.getLogger(RepositoryDeletedEventListener.class);

    @Inject
    private RemoteRepositoriesHeartbeatMonitorInitiator remoteRepositoriesHeartbeatMonitorInitiator;

    @EventListener
    public void handle(RepositoryEvent event) {
        if (event.getType() != RepositoryEventTypeEnum.EVENT_REPOSITORY_DELETED.getType()) {
            return;
        }
        removeRemoteRepository(ConfigurationUtils.getStorageIdAndRepositoryId(event.getStorageId(), event.getRepositoryId()));
    }

    private void removeRemoteRepository(String storageIdAndRepositoryId) {
        if (StringUtils.isNotBlank(storageIdAndRepositoryId)) {
            logger.info("Remote repository [{}] heartbeat monitor will be delete", storageIdAndRepositoryId);
            remoteRepositoriesHeartbeatMonitorInitiator.cancelRemoteRepositoryMonitoring(storageIdAndRepositoryId);
        }
    }
}
