package com.veadan.folib.components.promotion;

import com.veadan.folib.providers.AbstractMappedProviderRegistry;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

import javax.annotation.PostConstruct;

/**
 * @author veadan
 **/
@Slf4j
@Component
public class ArtifactPromotionProviderRegistry extends AbstractMappedProviderRegistry<ArtifactPromotionProvider> {

    @Override
    @PostConstruct
    public void initialize() {
        log.info("Initialized the promotion repository provider registry.");
    }
}
