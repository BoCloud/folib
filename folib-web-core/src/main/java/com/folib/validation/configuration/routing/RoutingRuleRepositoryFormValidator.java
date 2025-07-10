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
package com.folib.validation.configuration.routing;

import com.folib.forms.routing.RoutingRuleRepositoryForm;
import com.folib.services.ConfigurationManagementService;


import jakarta.inject.Inject;
import jakarta.validation.ConstraintValidator;
import jakarta.validation.ConstraintValidatorContext;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class RoutingRuleRepositoryFormValidator
        implements ConstraintValidator<RoutingRuleRepositoryFormValid, RoutingRuleRepositoryForm>
{

    private final Logger logger = LoggerFactory.getLogger(RoutingRuleRepositoryFormValidator.class);

    @Inject
    private ConfigurationManagementService configurationManagementService;

    private String message;

    @Override
    public void initialize(RoutingRuleRepositoryFormValid constraintAnnotation)
    {
        message = constraintAnnotation.message();
    }

    @Override
    public boolean isValid(RoutingRuleRepositoryForm form,
                           ConstraintValidatorContext context)
    {
        Boolean valid = false;

        try
        {
            String storageIdValue = StringUtils.trimToNull(form.getStorageId());
            String repositoryIdValue = StringUtils.trimToNull(form.getRepositoryId());

            if (storageIdValue == null && repositoryIdValue == null)
            {
                context.disableDefaultConstraintViolation();
                context.buildConstraintViolationWithTemplate(message)
                       .addPropertyNode("storageId")
                       .addConstraintViolation();
                context.buildConstraintViolationWithTemplate(message)
                       .addPropertyNode("repositoryId")
                       .addConstraintViolation();
            }
            else
            {
                valid = true;
            }
        }
        catch (Exception e)
        {
            logger.error(e.getMessage(), e);
        }

        return valid;
    }
}
