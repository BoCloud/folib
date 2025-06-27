package com.veadan.folib.components.webhook;

import com.veadan.folib.enums.WebhookEventsTypeEnum;
import com.veadan.folib.promotion.PromotionUtil;
import com.veadan.folib.providers.io.RepositoryPathResolver;
import com.veadan.folib.services.ArtifactManagementService;
import com.veadan.folib.services.ArtifactResolutionService;
import com.veadan.folib.services.WebhookEventsLogService;
import com.veadan.folib.utils.SecurityUtils;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import javax.annotation.PostConstruct;

/**
 * @author veadan
 * @date 2025/3/7
 **/
@Slf4j
@Component
public class GeneralWebhooksEventProvider extends BaseWebhookEventsProvider {

    @Autowired
    private WebhookEventsProviderRegistry webhookEventsProviderRegistry;

    @Autowired
    public GeneralWebhooksEventProvider(RepositoryPathResolver repositoryPathResolver, ArtifactResolutionService artifactResolutionService, ArtifactManagementService artifactManagementService, SecurityUtils securityUtils, WebhookEventsLogService webhookEventsLogService, PromotionUtil promotionUtil) {
        super(repositoryPathResolver, artifactResolutionService, artifactManagementService, securityUtils, webhookEventsLogService, promotionUtil);
    }

    @Override
    @PostConstruct
    public void register() {
        webhookEventsProviderRegistry.addProvider(WebhookEventsTypeEnum.GENERAL.getType(), this);
        log.info("Registered webhook events '{}' with alias '{}'.",
                getClass().getCanonicalName(), WebhookEventsTypeEnum.GENERAL.getType());
    }
}
