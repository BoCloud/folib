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
package com.folib.converters.storage.routing;

import com.folib.forms.routing.RoutingRuleForm;
import com.folib.storage.routing.MutableRoutingRule;
import com.folib.storage.routing.MutableRoutingRuleRepository;

import java.util.stream.Collectors;

import org.apache.commons.lang3.StringUtils;
import org.springframework.core.convert.converter.Converter;

/**
 * @author Veadan
 * @author veadan
 */
public class RoutingRuleFormToMutableConverter
        implements Converter<RoutingRuleForm, MutableRoutingRule>
{

    @Override
    public MutableRoutingRule convert(RoutingRuleForm routingRuleForm)
    {
        MutableRoutingRule rule = new MutableRoutingRule();
        rule.setGroupRepositoryId(StringUtils.trimToEmpty(routingRuleForm.getGroupRepositoryId()));
        rule.setStorageId(StringUtils.trimToEmpty(routingRuleForm.getStorageId()));
        rule.setType(routingRuleForm.getType().getType());
        rule.setPattern(routingRuleForm.getPattern());
        rule.setRepositories(
                routingRuleForm.getRepositories()
                               .stream()
                               .map(r -> {
                                   MutableRoutingRuleRepository repository = new MutableRoutingRuleRepository();
                                   repository.setRepositoryId(StringUtils.trimToEmpty(r.getRepositoryId()));
                                   repository.setStorageId(StringUtils.trimToEmpty(r.getStorageId()));
                                   return repository;
                               })
                               .collect(Collectors.toList())
        );

        return rule;
    }
}
