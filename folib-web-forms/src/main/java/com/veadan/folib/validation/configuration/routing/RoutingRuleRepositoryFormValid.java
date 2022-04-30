package com.veadan.folib.validation.configuration.routing;

import javax.validation.Constraint;
import javax.validation.Payload;
import java.lang.annotation.*;

@Documented
@Target(ElementType.TYPE)
@Retention(RetentionPolicy.RUNTIME)
@Constraint(validatedBy = {})
public @interface RoutingRuleRepositoryFormValid
{
    String message() default "Either storageId or repositoryId must not be blank!";

    Class<?>[] groups() default {};

    Class<? extends Payload>[] payload() default {};
}
