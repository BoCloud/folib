package com.folib.validation.users;


import jakarta.validation.Constraint;
import jakarta.validation.Payload;

import java.lang.annotation.*;

@Target({ ElementType.FIELD,
          ElementType.ANNOTATION_TYPE })
@Retention(RetentionPolicy.RUNTIME)
@Repeatable(Password.List.class)
@Documented
@Constraint(validatedBy = {})
public @interface Password
{

    String message() default "Password field is required!";

    String minMessage() default "Password has to be more than {} characters!";

    String maxMessage() default "Password has to be less than {} characters!";

    Class<?>[] groups() default {};

    Class<? extends Payload>[] payload() default {};

    int min() default 0;

    int max() default 2147483647;

    boolean allowNull() default false;

    @Target({ ElementType.FIELD,
              ElementType.ANNOTATION_TYPE })
    @Retention(RetentionPolicy.RUNTIME)
    @Documented
    public @interface List
    {
        Password[] value();
    }
}
