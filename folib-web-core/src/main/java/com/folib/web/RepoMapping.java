package com.folib.web;

import java.lang.annotation.Documented;
import java.lang.annotation.Retention;
import java.lang.annotation.Target;

import static java.lang.annotation.ElementType.PARAMETER;
import static java.lang.annotation.RetentionPolicy.RUNTIME;

/**
 * This annotation refers to a controller method parameter related to a Repository which is going to be validated
 * for provided path variables.
 *
 */
@Documented
@Retention(RUNTIME)
@Target(PARAMETER)
public @interface RepoMapping
{

    String storageVariableName() default "storageId";

    String repositoryVariableName() default "repositoryId";

    boolean allowOutOfServiceRepository() default false;
}
