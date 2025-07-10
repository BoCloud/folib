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
package com.folib.artifact.coordinates;

import com.folib.util.ThrowingFunction;
import org.reflections.ReflectionUtils;

import java.beans.BeanInfo;
import java.beans.Introspector;
import java.beans.PropertyDescriptor;
import java.lang.reflect.AnnotatedElement;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.Set;

public class ArtifactLayoutDescription
{

    private String layoutName;

    private String layoutAlias;

    private Class<? extends ArtifactCoordinates> artifactCoordinatesClass;

    private Set<String> artifactCoordinates = new HashSet<>();

    public String getLayoutName()
    {
        return layoutName;
    }

    public void setLayoutName(String layoutName)
    {
        this.layoutName = layoutName;
    }

    public String getLayoutAlias()
    {
        return layoutAlias;
    }

    public void setLayoutAlias(String layoutAlias)
    {
        this.layoutAlias = layoutAlias;
    }

    public Class<? extends ArtifactCoordinates> getArtifactCoordinatesClass()
    {
        return artifactCoordinatesClass;
    }

    public void setArtifactCoordinatesClass(Class<? extends ArtifactCoordinates> artifactCoordinatesClass)
    {
        this.artifactCoordinatesClass = artifactCoordinatesClass;
    }

    public Set<String> getArtifactCoordinates()
    {
        return Collections.unmodifiableSet(artifactCoordinates);
    }

    public static ArtifactLayoutDescription parse(Class<? extends ArtifactCoordinates> artifactCoordinatesClass)
    {
        ArtifactLayoutDescription result = ReflectionUtils.getAllAnnotations(artifactCoordinatesClass)
                                                          .stream()
                                                          .filter(a -> a instanceof CoordinatesLayout)
                                                          .findFirst()
                                                          .map(a -> (CoordinatesLayout) a)
                                                          .map(a -> parseClass(artifactCoordinatesClass, a))
                                                          .get();

        BeanInfo beanInfo;

        beanInfo = ThrowingFunction.unchecked((Class x) -> Introspector.getBeanInfo(x)).apply(artifactCoordinatesClass);


        Arrays.stream(beanInfo.getPropertyDescriptors())
              .map(p -> parseProperty(p))
              .filter(c -> c != null)
              .forEach(c -> result.artifactCoordinates.add(c));

        return result;
    }

    private static ArtifactLayoutDescription parseClass(Class<? extends ArtifactCoordinates> c,
                                                        CoordinatesLayout a)
    {
        ArtifactLayoutDescription result = new ArtifactLayoutDescription();

        result.setArtifactCoordinatesClass(c);

        String layoutName;
        if (!(layoutName = a.value()).isEmpty())
        {
            result.setLayoutName(layoutName);
        }
        else if (!(layoutName = a.name()).isEmpty())
        {
            result.setLayoutName(layoutName);
        }
        else
        {
            result.setLayoutName(c.getSimpleName());
        }

        String layoutAlias;
        if (!(layoutAlias = a.alias()).isEmpty())
        {
            result.setLayoutAlias(layoutAlias);
        }
        else
        {
            result.setLayoutAlias(result.getLayoutName());
        }

        return result;
    }

    private static String parseProperty(PropertyDescriptor p)
    {
        String propertyName = p.getName();
        Method propertyReadMethod = p.getReadMethod();
        Class<?> propertyOwnerClass = propertyReadMethod.getDeclaringClass();
        Field propertyField;
        try
        {
            propertyField = propertyOwnerClass.getDeclaredField(propertyName);
        }
        catch (NoSuchFieldException e)
        {
            propertyField = null;
        }

        String artifactCoordinateName;
        if ((artifactCoordinateName = calculateArtifactLayoutCoordinate(propertyName, propertyReadMethod)) != null)
        {
            return artifactCoordinateName;
        }
        else if (propertyField != null
                && (artifactCoordinateName = calculateArtifactLayoutCoordinate(propertyName, propertyField)) != null)
        {
            return artifactCoordinateName;
        }

        return null;
    }

    private static String calculateArtifactLayoutCoordinate(String elementName,
                                                            AnnotatedElement element)
    {
        ArtifactLayoutCoordinate artifactCoordinate = element.getAnnotation(ArtifactLayoutCoordinate.class);

        if (artifactCoordinate == null)
        {
            return null;
        }

        String artifactCoordinateName;
        if ((artifactCoordinateName = artifactCoordinate.name()).isEmpty()
                && (artifactCoordinateName = artifactCoordinate.value()).isEmpty())
        {
            return elementName;
        }

        return artifactCoordinateName;
    }

}
