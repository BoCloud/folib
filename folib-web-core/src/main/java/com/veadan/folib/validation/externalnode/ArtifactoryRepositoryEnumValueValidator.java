package com.veadan.folib.validation.externalnode;

import com.veadan.folib.enums.ArtifactoryRepositoryTypeEnum;
import jakarta.validation.ConstraintValidator;
import jakarta.validation.ConstraintValidatorContext;
import java.util.Arrays;

/**
 * @author leipenghui
 */
public class ArtifactoryRepositoryEnumValueValidator
        implements ConstraintValidator<ArtifactoryRepositoryEnumValue, String> {

    private Class<ArtifactoryRepositoryTypeEnum> type;

    private boolean allowNull;

    @Override
    public void initialize(final ArtifactoryRepositoryEnumValue constraintAnnotation) {
        type = constraintAnnotation.type();
        allowNull = constraintAnnotation.allowNull();
    }

    @Override
    public boolean isValid(final String value,
                           final ConstraintValidatorContext context) {
        if (value == null) {
            return allowNull;
        }

        return Arrays.stream(type.getEnumConstants())
                .map(ArtifactoryRepositoryTypeEnum::getType)
                .anyMatch(s -> s.equals(value));
    }
}
