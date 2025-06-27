package com.veadan.folib.components.syncartifact;

import com.veadan.folib.providers.AbstractMappedProviderRegistry;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

import javax.annotation.PostConstruct;

/**
 * @author veadan
 **/
@Slf4j
@Component
public class SyncArtifactProviderRegistry extends AbstractMappedProviderRegistry<SyncArtifactProvider> {

    @Override
    @PostConstruct
    public void initialize() {
        log.info("Initialized the sync repository provider registry.");
    }
}
