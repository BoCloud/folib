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
package com.folib.storage.validation;

import com.folib.artifact.coordinates.ArtifactCoordinates;
import com.folib.storage.validation.artifact.ArtifactCoordinatesValidationException;
import com.folib.storage.validation.artifact.version.VersionValidationException;
import com.folib.providers.ProviderImplementationException;
import com.folib.storage.repository.Repository;

import java.io.IOException;
import java.util.Collections;
import java.util.Set;

/**
 * @author veadan
 */
public interface ArtifactCoordinatesValidator
{

    void register();

    String getAlias();

    String getDescription();

    default boolean supports(Repository repository)
    {
        return true;
    }

    default boolean supports(String layoutProvider)
    {
        return true;
    }

    /**
     * Checks if an artifact version is acceptable by the repository.
     *
     * @param repository  The repository.
     * @param coordinates The artifact being deployed.
     */
    void validate(Repository repository,
                  ArtifactCoordinates coordinates)
            throws VersionValidationException,
                   ProviderImplementationException,
            ArtifactCoordinatesValidationException,
                   IOException;

    /**
     * Returns the list of supported validators. By default, it returns an empty set, meaning that there is
     * no explicit list of supported providers to be limited to, (hence it accepts any provider).
     *
     * @return
     */
    default Set<String> getSupportedLayoutProviders()
    {
        return Collections.emptySet();
    }

}
