package com.veadan.folib.validation.cron;

import jakarta.validation.Constraint;
import jakarta.validation.Payload;
import java.lang.annotation.*;

/**
 * @author veadan
 */
@Documented
@Constraint(validatedBy = {})
@Target(ElementType.TYPE)
@Retention(RetentionPolicy.RUNTIME)
public @interface CronTaskConfigurationDtoValid
{

    String message();

    Class<?>[] groups() default {};

    Class<? extends Payload>[] payload() default {};
}
