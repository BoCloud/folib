package com.veadan.folib.components.webhook;

import com.veadan.folib.providers.AbstractMappedProviderRegistry;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

import javax.annotation.PostConstruct;

/**
 * @author veadan
 **/
@Slf4j
@Component
public class WebhookEventsProviderRegistry extends AbstractMappedProviderRegistry<WebhookEventsProvider> {

    @Override
    @PostConstruct
    public void initialize() {
        log.info("Initialized the webhook events provider registry.");
    }
}
