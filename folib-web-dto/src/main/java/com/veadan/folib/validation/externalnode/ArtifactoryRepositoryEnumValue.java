package com.veadan.folib.validation.externalnode;

import com.veadan.folib.enums.ArtifactoryRepositoryTypeEnum;

import jakarta.validation.Constraint;
import jakarta.validation.Payload;
import java.lang.annotation.*;

/**
 * @author veadan
 */
@Documented
@Constraint(validatedBy = {})
@Target(ElementType.FIELD)
@Retention(RetentionPolicy.RUNTIME)
public @interface ArtifactoryRepositoryEnumValue {

    String message();

    Class<ArtifactoryRepositoryTypeEnum> type();

    boolean allowNull() default true;

    Class<?>[] groups() default {};

    Class<? extends Payload>[] payload() default {};
}
