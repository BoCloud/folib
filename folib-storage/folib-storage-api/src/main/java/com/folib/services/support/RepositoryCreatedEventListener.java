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
