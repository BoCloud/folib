package com.veadan.folib.validation.configuration;

import javax.validation.Constraint;
import javax.validation.Payload;
import java.lang.annotation.*;

/**
 * @author carlspring
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
