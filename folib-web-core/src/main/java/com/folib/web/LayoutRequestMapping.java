package com.folib.web;

import static java.lang.annotation.ElementType.TYPE;
import static java.lang.annotation.RetentionPolicy.RUNTIME;
import static com.folib.web.Constants.ARTIFACT_ROOT_PATH;

import java.lang.annotation.Documented;
import java.lang.annotation.Retention;
import java.lang.annotation.Target;

import org.springframework.web.bind.annotation.RequestMapping;

/**
 * This annotation maps Artifact Controller classes to `/storages` URL using
 * repository layout from folib storages configuration.
 * 
 * @author veadan
 * 
 * @see CustomRequestMappingHandlerMapping
 * @see LayoutRequestCondition
 */
@Documented
@Retention(RUNTIME)
@Target(TYPE)
@RequestMapping(ARTIFACT_ROOT_PATH)
public @interface LayoutRequestMapping
{

    /**
     * Layout Alias.
     */
    String value();

}
