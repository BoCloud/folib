package com.folib.validation.configuration;

import com.folib.api.Describable;
import jakarta.validation.ConstraintValidator;
import jakarta.validation.ConstraintValidatorContext;


import java.util.Arrays;

/**
 * @author veadan
 */
public class DescribableEnumValueValidator
        implements ConstraintValidator<DescribableEnumValue, String>
{

    private Class<? extends Describable> type;

    private boolean allowNull;

    @Override
    public void initialize(final DescribableEnumValue constraintAnnotation)
    {
        type = constraintAnnotation.type();
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

        return Arrays.stream(type.getEnumConstants())
                     .map(Describable::describe)
                     .filter(s -> s.equals(value))
                     .findFirst()
                     .isPresent();
    }
}
