package com.folib.job.tasks.cleanup;

import com.folib.providers.AbstractMappedProviderRegistry;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

import javax.annotation.PostConstruct;

/**
 * @author veadan
 **/
@Slf4j
@Component
public class CleanupArtifactsProviderRegistry extends AbstractMappedProviderRegistry<CleanupArtifactsProvider> {

    @Override
    @PostConstruct
    public void initialize() {
        log.info("Initialized the cleanup repository cron job provider registry.");
    }
}
