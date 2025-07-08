package com.folib.data.domain;

import com.fasterxml.jackson.annotation.JsonIgnore;

public interface EntityHierarchyNode<T extends EntityHierarchyNode<T>>
{
    @JsonIgnore
    default T getHierarchyChild()
    {
        return null;
    }

    default void setHierarchyChild(T node)
    {

    }
    @JsonIgnore
    default T getHierarchyParent()
    {
        return null;
    }

    default void setHierarchyParent(T node)
    {

    }

}
