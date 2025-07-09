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
package com.folib.converters.configuration;

import com.folib.storage.StorageDto;
import com.folib.storage.repository.RepositoryDto;
import com.folib.forms.configuration.RepositoryForm;
import com.folib.forms.configuration.StorageForm;

import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import org.springframework.core.convert.converter.Converter;

/**
 * @author veadan
 */
public enum StorageFormConverter
        implements Converter<StorageForm, StorageDto>
{
    INSTANCE;

    @Override
    public StorageDto convert(final StorageForm source)
    {
        StorageDto result = new StorageDto();
        result.setBasedir(source.getBasedir());
        result.setId(source.getId());
        result.setAdmin(source.getAdmin());
        result.setStorageProvider(source.getStorageProvider());
        result.setUsers(source.getUsers());
        List<RepositoryForm> repositories = source.getRepositories();
        if (repositories != null)
        {
            Map<String, RepositoryDto> internalMap = new LinkedHashMap<>();
            result.setRepositories(internalMap);
            repositories.stream()
                        .map(RepositoryFormConverter.INSTANCE::convert)
                        .forEach(mr -> internalMap.put(mr.getId(), mr));
        }
        return result;
    }
}
