package com.folib.gremlin.adapters;

import java.util.Set;

import com.folib.gremlin.dsl.EntityTraversal;
import com.folib.gremlin.dsl.__;
import org.apache.tinkerpop.gremlin.structure.Element;
import org.apache.tinkerpop.gremlin.structure.Vertex;
import com.folib.artifact.coordinates.GenericCoordinates;
import com.folib.db.schema.Edges;
import org.springframework.stereotype.Component;

/**
 * @author veadan
 */
@Component
public class ArtifactCoordinatesHierarchyAdapter
        extends EntityUpwardHierarchyAdapter<GenericCoordinates, ArtifactCoodrinatesNodeAdapter>
{

    public ArtifactCoordinatesHierarchyAdapter(Set<ArtifactCoodrinatesNodeAdapter> artifactAdapters)
    {
        super(artifactAdapters, 1);
    }

    @Override
    public EntityTraversal<Vertex, Element> cascade()
    {
        return __.<Vertex>aggregate("x")
                 .inE(Edges.EXTENDS)
                 .outV()
                 .aggregate("x")
                 .select("x")
                 .unfold();
    }

}
