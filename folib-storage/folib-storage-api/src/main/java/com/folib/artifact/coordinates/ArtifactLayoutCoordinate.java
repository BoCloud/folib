package com.folib.artifact.coordinates;

import static java.lang.annotation.ElementType.FIELD;
import static java.lang.annotation.ElementType.METHOD;
import static java.lang.annotation.RetentionPolicy.RUNTIME;

import java.lang.annotation.Documented;
import java.lang.annotation.Retention;
import java.lang.annotation.Target;

@Target({ FIELD, METHOD })
@Retention(RUNTIME)
@Documented
public @interface ArtifactLayoutCoordinate
{

    /**
     * Alias for "name".
     * 
     * @return
     */
    String value() default "";

    /**
     * Artifact coordinate name, default will be ArtifactCoordinates property
     * name.
     * 
     * @return
     */
    String name() default "";

}
