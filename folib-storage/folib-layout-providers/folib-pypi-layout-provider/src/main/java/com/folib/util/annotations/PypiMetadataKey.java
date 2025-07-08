package com.folib.util.annotations;

import javax.inject.Qualifier;
import java.lang.annotation.*;

@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.FIELD)
@Qualifier
@Documented
public @interface PypiMetadataKey
{

    public String name();

}
