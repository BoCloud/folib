/*
 * Folib - [新一代AI制品仓库]
 * Copyright (C) 2025 bocloud.com.cn <folib@beyondcent.com>
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * 本程序是自由软件：您可依据GNU通用公共许可证（GPL-3.0+）条款重新发布和修改，
 * 但禁止任何形式的商业售卖行为（包括但不限于：直接销售、捆绑销售、云服务商用）。
 *
 * This program is distributed WITHOUT ANY WARRANTY.
 * Commercial sale of this software is expressly prohibited.
 *
 * For license details, see: https://www.gnu.org/licenses/gpl-3.0.html
 * 商业授权咨询请联系：folib@beyondcent.com
 */
package com.folib.storage.validation.artifact;

import com.folib.storage.validation.ArtifactCoordinatesValidator;
import com.folib.providers.AbstractMappedProviderRegistry;

import javax.annotation.PostConstruct;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.Map;
import java.util.Set;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

/**
 * @author Veadan
 * @author Aditya Srinivasan
 */
@Component
public class ArtifactCoordinatesValidatorRegistry
        extends AbstractMappedProviderRegistry<ArtifactCoordinatesValidator>
{

    private static final Logger logger = LoggerFactory.getLogger(ArtifactCoordinatesValidatorRegistry.class);

    private Map<String, Set<ArtifactCoordinatesValidator>> validatorsByLayoutProvider = new LinkedHashMap<>();


    public ArtifactCoordinatesValidatorRegistry()
    {
    }

    @Override
    public ArtifactCoordinatesValidator addProvider(String alias, ArtifactCoordinatesValidator provider)
    {
        Set<String> supportedLayoutProviders = provider.getSupportedLayoutProviders();
        for (String layoutProvider : supportedLayoutProviders)
        {
            if (validatorsByLayoutProvider.containsKey(layoutProvider))
            {
                validatorsByLayoutProvider.get(layoutProvider).add(provider);
            }
            else
            {
                LinkedHashSet<ArtifactCoordinatesValidator> validators = new LinkedHashSet<>();
                validators.add(provider);

                validatorsByLayoutProvider.put(layoutProvider, validators);
            }
        }

        return super.addProvider(alias, provider);
    }

    @Override
    @PostConstruct
    public void initialize()
    {
        logger.info("Initialized the artifact coordinates validator registry.");
    }

    public Map<String, Set<ArtifactCoordinatesValidator>> getArtifactCoordinatesValidators()
    {
        return validatorsByLayoutProvider;
    }
}
