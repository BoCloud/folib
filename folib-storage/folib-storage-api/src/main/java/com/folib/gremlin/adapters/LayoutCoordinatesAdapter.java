package com.folib.gremlin.adapters;

import static com.folib.gremlin.dsl.EntityTraversalUtils.extractObject;

import java.util.Map;

import com.folib.gremlin.dsl.EntityTraversal;
import com.folib.gremlin.dsl.__;
import org.apache.tinkerpop.gremlin.process.traversal.Traverser;
import org.apache.tinkerpop.gremlin.structure.Element;
import org.apache.tinkerpop.gremlin.structure.Vertex;
import com.folib.artifact.coordinates.GenericCoordinates;
import com.folib.domain.LayoutCoordinatesEntity;

/**
 * @author veadan
 */
public abstract class LayoutCoordinatesAdapter<C extends LayoutCoordinatesEntity<C, V>, V extends Comparable<V>>
        implements VertexEntityTraversalAdapter<GenericCoordinates>, ArtifactCoodrinatesNodeAdapter
{
    private final String layoutCoorinatesLabel;
    private final Class<C> layoutCoordinatesClass;

    public LayoutCoordinatesAdapter(String label,
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
    public EntityTraversal<Vertex, GenericCoordinates> fold()
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
    public UnfoldEntityTraversal<Vertex, Vertex> unfold(GenericCoordinates entity)
    {
        return new UnfoldEntityTraversal<>(layoutCoorinatesLabel, entity, __.identity());
    }

    @Override
    public EntityTraversal<Vertex, Element> cascade()
    {
        throw new UnsupportedOperationException();
    }

}
