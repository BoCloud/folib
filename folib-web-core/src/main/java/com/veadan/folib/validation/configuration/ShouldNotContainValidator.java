package com.veadan.folib.validation.configuration;

import javax.validation.ConstraintValidator;
import javax.validation.ConstraintValidatorContext;
import java.util.Set;

import com.google.common.collect.ImmutableSet;

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
