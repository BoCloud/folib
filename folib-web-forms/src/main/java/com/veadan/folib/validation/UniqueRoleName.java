package com.veadan.folib.validation;

import javax.validation.Constraint;
import javax.validation.Payload;
import java.lang.annotation.*;

/**
 * @author Pablo Tirado
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
