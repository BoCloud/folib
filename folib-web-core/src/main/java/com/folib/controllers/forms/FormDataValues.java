/*
 * Folib - [新一代AI制品仓库]
 * Copyright (C) 2025 bocloud.com.cn <folib@beyondcent.com>
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * 本程序是自由软件：您可依据GNU通用公共许可证（GPL-3.0+）条款重新发布和修改，
 * 但禁止任何形式的商业售卖行为（包括但不限于：直接销售、捆绑销售、云服务商用）。
 *
 * This program is distributed WITHOUT ANY WARRANTY.
 * Commercial sale of this software is expressly prohibited.
 *
 * For license details, see: https://www.gnu.org/licenses/gpl-3.0.html
 * 商业授权咨询请联系：folib@beyondcent.com
 */
package com.folib.controllers.forms;

import com.folib.api.Describable;

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
