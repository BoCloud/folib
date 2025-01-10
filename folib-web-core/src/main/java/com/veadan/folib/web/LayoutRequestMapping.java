package com.veadan.folib.web;

import static com.veadan.folib.web.Constants.ARTIFACTORY_ARTIFACT_ROOT_PATH;
import static java.lang.annotation.ElementType.TYPE;
import static java.lang.annotation.RetentionPolicy.RUNTIME;
import static com.veadan.folib.web.Constants.ARTIFACT_ROOT_PATH;

import java.lang.annotation.Documented;
import java.lang.annotation.Retention;
import java.lang.annotation.Target;

import org.springframework.web.bind.annotation.RequestMapping;

/**
 * This annotation maps Artifact Controller classes to `/storages` URL using
 * repository layout from folib storages configuration.
 * 
 * @author xuxinping
 * 
 * @see CustomRequestMappingHandlerMapping
 * @see LayoutRequestCondition
 */
@Documented
@Retention(RUNTIME)
@Target(TYPE)
@RequestMapping(path = {ARTIFACT_ROOT_PATH, ARTIFACTORY_ARTIFACT_ROOT_PATH})
public @interface LayoutRequestMapping
{

    /**
     * Layout Alias.
     */
    String value();

}
