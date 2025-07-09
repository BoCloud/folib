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

import com.folib.providers.PypiLayoutProvider;
import com.folib.storage.repository.Repository;

import java.util.Set;

import com.google.common.collect.Sets;

/**
 * @author sainalshah
 */
public interface PypiArtifactCoordinatesValidator
        extends ArtifactCoordinatesValidator
{

    @Override
    default boolean supports(Repository repository)
    {
        return supports(repository.getLayout());
    }

    @Override
    default boolean supports(String layoutProvider)
    {
        return PypiLayoutProvider.ALIAS.equals(layoutProvider);
    }

    @Override
    default Set<String> getSupportedLayoutProviders()
    {
        return Sets.newHashSet(PypiLayoutProvider.ALIAS);
    }

}
