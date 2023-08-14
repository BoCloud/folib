package com.veadan.folib.validation.externalnode;

import com.veadan.folib.enums.ArtifactoryRepositoryTypeEnum;

import javax.validation.Constraint;
import javax.validation.Payload;
import java.lang.annotation.*;

/**
 * @author leipenghui
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
