package com.veadan.folib.validation.cron;

import com.veadan.folib.cron.jobs.CronJobDefinition;
import com.veadan.folib.cron.jobs.CronJobsDefinitionsRegistry;
import com.veadan.folib.cron.jobs.fields.CronJobField;
import com.veadan.folib.forms.cron.CronTaskConfigurationForm;
import com.veadan.folib.forms.cron.CronTaskConfigurationFormField;
import com.veadan.folib.validation.cron.autocomplete.CronTaskConfigurationFormFieldAutocompleteValidator;
import com.veadan.folib.validation.cron.autocomplete.CronTaskConfigurationFormFieldAutocompleteValidatorsRegistry;
import com.veadan.folib.validation.cron.type.CronTaskConfigurationFormFieldTypeValidator;
import com.veadan.folib.validation.cron.type.CronTaskConfigurationFormFieldTypeValidatorsRegistry;


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
