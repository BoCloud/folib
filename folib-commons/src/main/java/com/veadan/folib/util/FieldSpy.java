package com.veadan.folib.util;

import java.util.HashSet;
import java.util.Set;

import org.apache.commons.lang3.reflect.FieldUtils;

/**
 * @author Przemyslaw Fusik
 */
public class FieldSpy<T>
{

    public static Set<FieldInfo> getAllFieldsInfo(final Class<?> cls)
    {
        Set<FieldInfo> fields = new HashSet<>();
        FieldUtils.getAllFieldsList(cls)
                  .stream()
                  .forEach(field -> fields.add(new FieldInfo(field.getName(), field.getType().getTypeName())));

        return fields;
    }

    public static class FieldInfo
    {

        private String name;

        private String type;

        public FieldInfo(final String name,
                         final String type)
        {
            this.name = name;
            this.type = type;
        }

        public String getName()
        {
            return name;
        }

        public String getType()
        {
            return type;
        }
    }

}
