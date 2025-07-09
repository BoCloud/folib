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

import com.folib.storage.repository.RepositoryPermissionDto;
import com.folib.forms.configuration.RepositoryPermissionForm;
import com.folib.storage.repository.RepositoryPermissionUserDto;
import org.apache.commons.collections4.CollectionUtils;
import org.springframework.beans.BeanUtils;
import org.springframework.core.convert.converter.Converter;

import java.util.List;
import java.util.stream.Collectors;

/**
 * @author veadan
 */
public enum RepositoryPermissionFormConverter
        implements Converter<RepositoryPermissionForm, RepositoryPermissionDto> {
    INSTANCE;

    @Override
    public RepositoryPermissionDto convert(final RepositoryPermissionForm source) {
        RepositoryPermissionDto result = new RepositoryPermissionDto();
        result.setScope(source.getScope());
        result.setAllowAnonymous(source.isAllowAnonymous());
        if (CollectionUtils.isNotEmpty(source.getUserList())) {
            List<RepositoryPermissionUserDto> repositoryPermissionUserList = source.getUserList().stream().map(item -> {
                RepositoryPermissionUserDto repositoryPermissionUserDto = new RepositoryPermissionUserDto();
                BeanUtils.copyProperties(item, repositoryPermissionUserDto);
                return repositoryPermissionUserDto;
            }).collect(Collectors.toList());
            result.setUserList(repositoryPermissionUserList);
        }
        return result;
    }
}
