package com.veadan.folib.services.impl;

import com.veadan.folib.providers.AbstractMappedProviderRegistry;
import com.veadan.folib.services.PypiProvider;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

import javax.annotation.PostConstruct;

/**
 * @author leipenghui
 **/
@Slf4j
@Service
public class PypiProviderRegistry extends AbstractMappedProviderRegistry<PypiProvider> {

    @Override
    @PostConstruct
    public void initialize() {
        log.info("Initialized the pypi provider registry.");
    }
}
