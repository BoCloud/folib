package com.veadan.folib.services.impl;

import com.veadan.folib.providers.AbstractMappedProviderRegistry;
import com.veadan.folib.services.NpmProvider;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

import javax.annotation.PostConstruct;

/**
 * @author veadan
 **/
@Slf4j
@Service
public class NpmProviderRegistry extends AbstractMappedProviderRegistry<NpmProvider> {

    @Override
    @PostConstruct
    public void initialize() {
        log.info("Initialized the npm provider registry.");
    }
}
