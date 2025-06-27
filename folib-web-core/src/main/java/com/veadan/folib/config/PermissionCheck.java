package com.veadan.folib.config;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * @author veadan
 */
@Target({ElementType.METHOD, ElementType.TYPE})
@Retention(RetentionPolicy.RUNTIME)
public @interface PermissionCheck {
    /**
     * 资源key
     */
    String resourceKey();

    /**
     * storageKey
     */
    String storageKey() default "";

    /**
     * repositoryKey
     */
    String repositoryKey() default "";

    /**
     * pathKey
     */
    String pathKey() default "";
}