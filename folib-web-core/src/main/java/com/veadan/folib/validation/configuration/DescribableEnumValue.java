package com.veadan.folib.validation.configuration;

import com.veadan.folib.api.Describable;
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
public @interface DescribableEnumValue
{

    String message();

    Class<? extends Describable> type();

    boolean allowNull() default true;

    Class<?>[] groups() default {};

    Class<? extends Payload>[] payload() default {};
}
