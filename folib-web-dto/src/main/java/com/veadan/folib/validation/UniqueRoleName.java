package com.veadan.folib.validation;


import jakarta.validation.Constraint;
import jakarta.validation.Payload;

import java.lang.annotation.*;

/**
 * @author Veadan
 */
@Documented
@Constraint(validatedBy = {})
@Target({ ElementType.FIELD,
          ElementType.METHOD })
@Retention(RetentionPolicy.RUNTIME)
public @interface UniqueRoleName
{

    String message();

    Class<?>[] groups() default {};

    Class<? extends Payload>[] payload() default {};
}
