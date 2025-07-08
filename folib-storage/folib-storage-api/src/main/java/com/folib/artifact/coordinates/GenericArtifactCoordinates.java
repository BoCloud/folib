package com.folib.artifact.coordinates;

import java.util.Map;

import com.folib.data.domain.DomainObject;
import com.folib.data.domain.EntityHierarchyNode;

/**
 * @author veadan
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
