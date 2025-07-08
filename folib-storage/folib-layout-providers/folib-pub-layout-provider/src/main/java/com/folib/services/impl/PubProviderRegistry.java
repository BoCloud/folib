package com.folib.services.impl;

import com.folib.providers.AbstractMappedProviderRegistry;
import com.folib.services.PubProvider;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

import javax.annotation.PostConstruct;

/**
 * @author veadan
 **/
@Slf4j
@Service
public class PubProviderRegistry extends AbstractMappedProviderRegistry<PubProvider> {

    @Override
    @PostConstruct
    public void initialize() {
        log.info("Initialized the pub provider registry.");
    }
}
