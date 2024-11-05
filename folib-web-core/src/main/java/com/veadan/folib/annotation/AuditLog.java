package com.veadan.folib.annotation;

import com.veadan.folib.enums.AuditEventNameEnum;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * @author huayanjun
 * @since 2024-08-12 15:37
 */

@Target(ElementType.METHOD)
@Retention(RetentionPolicy.RUNTIME)
public @interface AuditLog {
    AuditEventNameEnum value();

    String target();

}
