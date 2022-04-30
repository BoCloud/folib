package com.veadan.folib.gremlin.adapters;

import static com.veadan.folib.gremlin.dsl.EntityTraversalUtils.extractObject;

import java.util.Map;

import com.veadan.folib.gremlin.dsl.EntityTraversal;
import com.veadan.folib.gremlin.dsl.__;
import org.apache.tinkerpop.gremlin.process.traversal.Traverser;
import org.apache.tinkerpop.gremlin.structure.Element;
import org.apache.tinkerpop.gremlin.structure.Vertex;
import com.veadan.folib.artifact.coordinates.GenericArtifactCoordinates;
import com.veadan.folib.domain.LayoutArtifactCoordinatesEntity;

/**
 * @author sbespalov
 */
public abstract class LayoutArtifactCoordinatesAdapter<C extends LayoutArtifactCoordinatesEntity<C, V>, V extends Comparable<V>>
        implements VertexEntityTraversalAdapter<GenericArtifactCoordinates>, ArtifactCoodrinatesNodeAdapter
{
    private final String layoutCoorinatesLabel;
    private final Class<C> layoutCoordinatesClass;

    public LayoutArtifactCoordinatesAdapter(String label,
                                            Class<C> layoutCoordinatesClass)
    {
        this.layoutCoorinatesLabel = label;
        this.layoutCoordinatesClass = layoutCoordinatesClass;
    }

    @Override
    public String label()
    {
        return layoutCoorinatesLabel;
    }

    @Override
    public Class<C> entityClass()
    {
        return layoutCoordinatesClass;
    }

    @Override
    public EntityTraversal<Vertex, GenericArtifactCoordinates> fold()
    {
        return __.<Vertex, Object>project("id", "uuid")
                 .by(__.id())
                 .by(__.enrichPropertyValue("uuid"))
                 .map(this::map);
    }

    private C map(Traverser<Map<String, Object>> t)
    {
        C result = newInstance();
        result.setNativeId(extractObject(Long.class, t.get().get("id")));
        result.setUuid(extractObject(String.class, t.get().get("uuid")));

        return result;
    }

    protected abstract C newInstance();

    @Override
    public UnfoldEntityTraversal<Vertex, Vertex> unfold(GenericArtifactCoordinates entity)
    {
        return new UnfoldEntityTraversal<>(layoutCoorinatesLabel, entity, __.identity());
    }

    @Override
    public EntityTraversal<Vertex, Element> cascade()
    {
        throw new UnsupportedOperationException();
    }

}
