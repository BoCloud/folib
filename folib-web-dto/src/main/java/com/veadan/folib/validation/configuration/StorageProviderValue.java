package com.veadan.folib.validation.configuration;

import jakarta.validation.Constraint;
import jakarta.validation.Payload;
import java.lang.annotation.*;

/**
 * @author Veadan
 */
@Documented
@Constraint(validatedBy = {})
@Target(ElementType.FIELD)
@Retention(RetentionPolicy.RUNTIME)
public @interface StorageProviderValue
{

    boolean allowNull() default true;

    String message();

    Class<?>[] groups() default {};

    Class<? extends Payload>[] payload() default {};

}
