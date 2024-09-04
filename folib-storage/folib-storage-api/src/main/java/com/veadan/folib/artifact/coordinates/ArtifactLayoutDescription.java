package com.veadan.folib.artifact.coordinates;

import com.veadan.folib.util.ThrowingFunction;
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
                                                          .filter(a -> a instanceof ArtifactCoordinatesLayout)
                                                          .findFirst()
                                                          .map(a -> (ArtifactCoordinatesLayout) a)
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
                                                        ArtifactCoordinatesLayout a)
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
        System.out.println(propertyName);
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
