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
package com.folib.validation.cron;

import com.folib.job.cron.jobs.CronJobDefinition;
import com.folib.job.cron.jobs.CronJobsDefinitionsRegistry;
import com.folib.job.cron.jobs.fields.CronJobField;
import com.folib.forms.cron.CronTaskConfigurationForm;
import com.folib.forms.cron.CronTaskConfigurationFormField;
import com.folib.validation.cron.autocomplete.CronTaskConfigurationFormFieldAutocompleteValidator;
import com.folib.validation.cron.autocomplete.CronTaskConfigurationFormFieldAutocompleteValidatorsRegistry;
import com.folib.validation.cron.type.CronTaskConfigurationFormFieldTypeValidator;
import com.folib.validation.cron.type.CronTaskConfigurationFormFieldTypeValidatorsRegistry;


import java.util.Arrays;
import java.util.Optional;
import java.util.stream.Collectors;

import jakarta.inject.Inject;
import jakarta.validation.ConstraintValidator;
import jakarta.validation.ConstraintValidatorContext;
import org.apache.commons.lang3.StringUtils;
import org.quartz.CronExpression;

/**
 * @author veadan
 */
public class CronTaskConfigurationFormValidator
        implements ConstraintValidator<CronTaskConfigurationFormValid, CronTaskConfigurationForm>
{

    @Inject
    private CronJobsDefinitionsRegistry cronJobsDefinitionsRegistry;

    @Inject
    private CronTaskConfigurationFormFieldTypeValidatorsRegistry cronTaskConfigurationFormFieldTypeValidatorsRegistry;

    @Inject
    private CronTaskConfigurationFormFieldAutocompleteValidatorsRegistry cronTaskConfigurationFormFieldAutocompleteValidatorsRegistry;

    @Override
    public boolean isValid(CronTaskConfigurationForm form,
                           ConstraintValidatorContext context)
    {

        CronJobDefinition cronJobDefinition;
        try
        {
            cronJobDefinition = getCorrespondingCronJobDefinition(form, context);
        }
        catch (CronTaskDefinitionFormValidatorException ex)
        {
            return false;
        }

        boolean isValid = true;
        boolean cronExpressionIsValid = true;

        if (form.isImmediateExecution() &&
            form.isOneTimeExecution() &&
            StringUtils.isNotBlank(form.getCronExpression()))
        {
            context.buildConstraintViolationWithTemplate(
                    "Cron expression should not be provided when both immediateExecution and oneTimeExecution are set to true")
                   .addPropertyNode("cronExpression")
                   .addConstraintViolation();
            isValid = false;
            cronExpressionIsValid = false;
        }

        if (cronExpressionIsValid && StringUtils.isBlank(form.getCronExpression()))
        {
            context.buildConstraintViolationWithTemplate(
                    "Cron expression is required")
                   .addPropertyNode("cronExpression")
                   .addConstraintViolation();
            isValid = false;
            cronExpressionIsValid = false;
        }

        if (cronExpressionIsValid && !CronExpression.isValidExpression(form.getCronExpression()))
        {
            context.buildConstraintViolationWithTemplate(
                    "Cron expression is invalid")
                   .addPropertyNode("cronExpression")
                   .addConstraintViolation();
            isValid = false;
        }

        for (CronJobField definitionField : cronJobDefinition.getFields())
        {
            String definitionFieldName = definitionField.getName();
            CronTaskConfigurationFormField correspondingFormField = null;
            int correspondingFormFieldIndex = -1;
            for (int i = 0; i < form.getFields().size(); i++)
            {
                CronTaskConfigurationFormField formField = form.getFields().get(i);

                String formFieldName = formField.getName();
                if (StringUtils.equals(definitionFieldName, formFieldName))
                {
                    correspondingFormField = formField;
                    correspondingFormFieldIndex = i;
                    break;
                }
            }
            if (correspondingFormField == null)
            {
                if (definitionField.isRequired())
                {
                    context.buildConstraintViolationWithTemplate(
                            String.format("Required field [%s] not provided", definitionFieldName))
                           .addPropertyNode("fields")
                           .addConstraintViolation();
                    isValid = false;
                }
                // field is not required and is not provided
                continue;
            }

            String formFieldValue = correspondingFormField.getValue();
            if (StringUtils.isBlank(formFieldValue) && definitionField.isRequired())
            {
                context.buildConstraintViolationWithTemplate(
                        String.format("Required field value [%s] not provided", definitionFieldName))
                       .addPropertyNode("fields")
                       .addPropertyNode("value")
                       .inIterable().atIndex(correspondingFormFieldIndex)
                       .addConstraintViolation();
                isValid = false;
                continue;
            }

            String definitionFieldType = definitionField.getType();
            CronTaskConfigurationFormFieldTypeValidator cronTaskConfigurationFormFieldTypeValidator = cronTaskConfigurationFormFieldTypeValidatorsRegistry.get(
                    definitionFieldType);
            if (!cronTaskConfigurationFormFieldTypeValidator.isValid(formFieldValue))
            {
                context.buildConstraintViolationWithTemplate(
                        String.format("Invalid value [%s] type provided. [%s] was expected.", escapeMessageValue(formFieldValue),
                                      definitionFieldType))
                       .addPropertyNode("fields")
                       .addPropertyNode("value")
                       .inIterable().atIndex(correspondingFormFieldIndex)
                       .addConstraintViolation();
                isValid = false;
                continue;
            }

            String autocompleteValue = definitionField.getAutocompleteValue();

            if (autocompleteValue != null)
            {
                CronTaskConfigurationFormFieldAutocompleteValidator cronTaskConfigurationFormFieldAutocompleteValidator = cronTaskConfigurationFormFieldAutocompleteValidatorsRegistry.get(
                        autocompleteValue);
                if (!cronTaskConfigurationFormFieldAutocompleteValidator.isValid(formFieldValue))
                {
                    context.buildConstraintViolationWithTemplate(
                            String.format("Invalid value [%s] provided. Possible values do not contain this value.",
                                          escapeMessageValue(formFieldValue)))
                           .addPropertyNode("fields")
                           .addPropertyNode("value")
                           .inIterable().atIndex(correspondingFormFieldIndex)
                           .addConstraintViolation();
                    isValid = false;
                    continue;
                }
            }
            // TODO SB-1393
        }

        return isValid;
    }

    private String escapeMessageValue(String value)
    {
        return Arrays.stream(value.split("\\$")).collect(Collectors.joining("\\$"));
    }

    private CronJobDefinition getCorrespondingCronJobDefinition(CronTaskConfigurationForm form,
                                                                ConstraintValidatorContext context)
    {
        String id = StringUtils.trimToEmpty(form.getJobClass());
        Optional<CronJobDefinition> cronJobDefinition = cronJobsDefinitionsRegistry.get(id);
        return cronJobDefinition.orElseThrow(() ->
                                             {
                                                 context.buildConstraintViolationWithTemplate(
                                                         "Cron job not found")
                                                        .addPropertyNode("jobClass")
                                                        .addConstraintViolation();
                                                 return new CronTaskDefinitionFormValidatorException();
                                             }
        );
    }

    static class CronTaskDefinitionFormValidatorException
            extends RuntimeException
    {

    }
}
