package com.veadan.folib.validation.cron;

import javax.validation.Constraint;
import javax.validation.Payload;
import java.lang.annotation.*;

/**
 * @author Przemyslaw Fusik
 */
@Documented
@Constraint(validatedBy = {})
@Target(ElementType.TYPE)
@Retention(RetentionPolicy.RUNTIME)
public @interface CronTaskConfigurationFormValid
{

    String message();

    Class<?>[] groups() default {};

    Class<? extends Payload>[] payload() default {};
}
