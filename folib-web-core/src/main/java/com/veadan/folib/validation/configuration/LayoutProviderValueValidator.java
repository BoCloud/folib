package com.veadan.folib.validation.configuration;

import com.veadan.folib.providers.layout.LayoutProviderRegistry;
import jakarta.inject.Inject;
import jakarta.validation.ConstraintValidator;
import jakarta.validation.ConstraintValidatorContext;


/**
 * @author veadan
 */
public class LayoutProviderValueValidator
        implements ConstraintValidator<LayoutProviderValue, String>
{

    private boolean allowNull;

    @Inject
    private LayoutProviderRegistry layoutProviderRegistry;

    @Override
    public void initialize(final LayoutProviderValue constraintAnnotation)
    {
        allowNull = constraintAnnotation.allowNull();
    }

    @Override
    public boolean isValid(final String value,
                           final ConstraintValidatorContext context)
    {
        if (value == null)
        {
            return allowNull;
        }

        return layoutProviderRegistry.getProviders().keySet().contains(value);
    }
}
