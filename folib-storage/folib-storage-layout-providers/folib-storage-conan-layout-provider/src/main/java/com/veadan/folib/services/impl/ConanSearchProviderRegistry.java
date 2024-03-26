package com.veadan.folib.services.impl;

import com.veadan.folib.providers.AbstractMappedProviderRegistry;
import com.veadan.folib.services.ConanSearchProvider;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

import javax.annotation.PostConstruct;

/**
 * @author leipenghui
 **/
@Slf4j
@Service
public class ConanSearchProviderRegistry extends AbstractMappedProviderRegistry<ConanSearchProvider> {

    @Override
    @PostConstruct
    public void initialize() {
        log.info("Initialized the conan search provider registry.");
    }
}
