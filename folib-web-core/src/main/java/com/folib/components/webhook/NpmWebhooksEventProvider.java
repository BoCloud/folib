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
package com.folib.components.webhook;

import com.folib.artifact.coordinates.NpmCoordinates;
import com.folib.controllers.adapter.jfrog.dto.WebhookDto;
import com.folib.entity.Dict;
import com.folib.enums.WebhookEventsTypeEnum;
import com.folib.promotion.PromotionUtil;
import com.folib.providers.io.RepositoryPath;
import com.folib.providers.io.RepositoryPathResolver;
import com.folib.services.ArtifactManagementService;
import com.folib.services.ArtifactResolutionService;
import com.folib.services.WebhookEventsLogService;
import com.folib.utils.SecurityUtils;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import javax.annotation.PostConstruct;

/**
 * @author leipenghui
 * @date 2025/3/7
 **/
@Slf4j
@Component
public class NpmWebhooksEventProvider extends BaseWebhookEventsProvider {

    @Autowired
    private WebhookEventsProviderRegistry webhookEventsProviderRegistry;

    @Autowired
    public NpmWebhooksEventProvider(RepositoryPathResolver repositoryPathResolver, ArtifactResolutionService artifactResolutionService, ArtifactManagementService artifactManagementService, SecurityUtils securityUtils, WebhookEventsLogService webhookEventsLogService, PromotionUtil promotionUtil) {
        super(repositoryPathResolver, artifactResolutionService, artifactManagementService, securityUtils, webhookEventsLogService, promotionUtil);
    }

    @Override
    @PostConstruct
    public void register() {
        webhookEventsProviderRegistry.addProvider(WebhookEventsTypeEnum.NPM.getType(), this);
        log.info("Registered webhook events '{}' with alias '{}'.",
                getClass().getCanonicalName(), WebhookEventsTypeEnum.NPM.getType());
    }

    @Override
    public boolean handler(WebhookDto webhook, RepositoryPath repositoryPath, Dict artifactMigrateInfo, int type) {
        repositoryPath = repositoryPath.getRoot().resolve(resolvePath(webhook));
        return super.handler(webhook, repositoryPath, artifactMigrateInfo, type);
    }

    @Override
    public String resolvePath(WebhookDto webhook) {
        NpmCoordinates npmArtifactCoordinates = NpmCoordinates.parseByResolvePath(webhook.getData().getPath());
        return npmArtifactCoordinates.buildPath();
    }
}
