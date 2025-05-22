package com.veadan.folib.validation.configuration;


import java.util.Set;

import com.google.common.collect.ImmutableSet;
import jakarta.validation.ConstraintValidator;
import jakarta.validation.ConstraintValidatorContext;

public class ShouldNotContainValidator
        implements ConstraintValidator<ShouldNotContain, String>
{

    private Set<String> strings;

    @Override
    public void initialize(ShouldNotContain constraint)
    {
        strings = ImmutableSet.copyOf(constraint.strings());
    }

    @Override
    public boolean isValid(String value,
                           ConstraintValidatorContext context)
    {
        if (value == null)
        {
            return true;
        }

        return !strings.stream()
                       .filter(disallowed -> value.contains(disallowed))
                       .findFirst()
                       .isPresent();
    }

}
