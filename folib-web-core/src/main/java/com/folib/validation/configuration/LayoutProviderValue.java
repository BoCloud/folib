package com.folib.validation.configuration;


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
public @interface LayoutProviderValue
{

    boolean allowNull() default true;

    String message();

    Class<?>[] groups() default {};

    Class<? extends Payload>[] payload() default {};
}
