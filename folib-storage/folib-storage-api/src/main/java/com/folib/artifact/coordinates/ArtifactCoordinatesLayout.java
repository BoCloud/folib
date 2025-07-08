package com.folib.artifact.coordinates;

import static java.lang.annotation.ElementType.TYPE;
import static java.lang.annotation.RetentionPolicy.RUNTIME;

import java.lang.annotation.Documented;
import java.lang.annotation.Retention;
import java.lang.annotation.Target;

@Target(TYPE)
@Retention(RUNTIME)
@Documented
public @interface ArtifactCoordinatesLayout
{

    /**
     * Alias for "name".
     * 
     * @return
     */
    String value() default "";

    /**
     * Layout full name (Maven2, NuGet etc.), default will be
     * ArtifactCoordinates class simple name.
     * 
     * @return
     */
    String name() default "";

    /**
     * Layout short name to be used within AQL (maven, nuget etc.)
     * 
     * @return
     */
    String alias() default "";

}
