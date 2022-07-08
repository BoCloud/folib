package com.veadan.folib.controllers.forms;

import com.veadan.folib.api.Describable;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.stream.Collectors;

/**
 * @author veadan
 */
public class FormDataValues<T>
{

    private String name;

    private List<T> values;

    public static FormDataValues<String> fromDescribableEnum(final String name,
                                                             final Class<? extends Describable> describableEnum)
    {
        final FormDataValues<String> result = new FormDataValues<>();
        result.setName(name);
        result.setValues(Arrays.stream(describableEnum.getEnumConstants()).map(Describable::describe).collect(
                Collectors.toList()));
        return result;
    }

    public static FormDataValues fromCollection(final String name,
                                                final Collection<?> values)
    {
        final FormDataValues<String> result = new FormDataValues<>();
        result.setName(name);
        result.setValues(new ArrayList(values));
        return result;
    }

    public String getName()
    {
        return name;
    }

    public void setName(final String name)
    {
        this.name = name;
    }

    public List<T> getValues()
    {
        return values;
    }

    public void setValues(final List<T> values)
    {
        this.values = values;
    }
}
