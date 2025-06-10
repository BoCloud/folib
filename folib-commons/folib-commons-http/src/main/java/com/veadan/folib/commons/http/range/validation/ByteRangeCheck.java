package com.veadan.folib.commons.http.range.validation;

import com.veadan.folib.commons.http.range.validation.ByteRangeCheckValidator;

import javax.validation.Constraint;
import javax.validation.Payload;
import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * @author Pablo Tirado
 */
@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.TYPE)
@Constraint(validatedBy = ByteRangeCheckValidator.class)
public @interface ByteRangeCheck
{

    String message();

    Class<?>[] groups() default {};

    Class<? extends Payload>[] payload() default {};
}
