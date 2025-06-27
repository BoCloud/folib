package com.veadan.folib.annotation;

import java.lang.annotation.*;

/**
 * @author veadan
 * @date 2023/6/27
 **/
@Documented
@Inherited
@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.METHOD)
public @interface LicenseAnnotation {
}
