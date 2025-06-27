package com.veadan.folib.services.impl;

import com.veadan.folib.providers.AbstractMappedProviderRegistry;
import com.veadan.folib.services.ConanProvider;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

import javax.annotation.PostConstruct;

/**
 * @author veadan
 **/
@Slf4j
@Service
public class ConanProviderRegistry extends AbstractMappedProviderRegistry<ConanProvider> {

    @Override
    @PostConstruct
    public void initialize() {
        log.info("Initialized the conan provider registry.");
    }
}
