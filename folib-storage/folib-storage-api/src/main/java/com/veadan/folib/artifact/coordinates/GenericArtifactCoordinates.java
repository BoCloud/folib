package com.veadan.folib.artifact.coordinates;

import java.util.Map;

import com.veadan.folib.data.domain.DomainObject;
import com.veadan.folib.data.domain.EntityHierarchyNode;

/**
 * @author xuxinping
 *
 */
public interface GenericArtifactCoordinates extends DomainObject, EntityHierarchyNode<GenericArtifactCoordinates>
{

    String getVersion();

    Map<String, String> getCoordinates();

    default String getPath()
    {
        return getUuid();
    }

}
