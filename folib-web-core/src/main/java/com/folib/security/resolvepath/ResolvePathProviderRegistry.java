package com.folib.security.resolvepath;

import com.folib.providers.AbstractMappedProviderRegistry;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

import javax.annotation.PostConstruct;

/**
 * @author veadan
 **/
@Slf4j
@Component
public class ResolvePathProviderRegistry extends AbstractMappedProviderRegistry<ResolvePathProvider> {

    @Override
    @PostConstruct
    public void initialize() {
        log.info("Initialized the resolve path provider registry.");
    }
}
