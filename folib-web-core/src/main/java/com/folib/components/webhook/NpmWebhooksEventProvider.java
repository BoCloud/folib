package com.folib.components.webhook;

import com.folib.artifact.coordinates.NpmArtifactCoordinates;
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
        NpmArtifactCoordinates npmArtifactCoordinates = NpmArtifactCoordinates.parseByResolvePath(webhook.getData().getPath());
        return npmArtifactCoordinates.buildPath();
    }
}
