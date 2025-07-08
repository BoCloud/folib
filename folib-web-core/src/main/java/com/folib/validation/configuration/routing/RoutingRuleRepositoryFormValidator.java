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
