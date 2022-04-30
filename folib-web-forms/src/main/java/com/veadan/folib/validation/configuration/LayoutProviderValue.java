package com.veadan.folib.validation.configuration;

import javax.validation.Constraint;
import javax.validation.Payload;
import java.lang.annotation.Documented;
import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * @author Przemyslaw Fusik
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
