package com.veadan.folib.log;

import com.google.common.base.CaseFormat;

public class LoggingUtils
{

    public static String caclucateCronContextName(Class<?> contextClass)
    {
        return String.format("folib-%s",
                             CaseFormat.UPPER_CAMEL.to(CaseFormat.LOWER_HYPHEN, contextClass.getSimpleName()));
    }

}
