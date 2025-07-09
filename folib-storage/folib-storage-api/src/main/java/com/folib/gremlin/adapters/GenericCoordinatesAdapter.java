package com.folib.gremlin.adapters;


import com.folib.artifact.coordinates.GenericCoordinates;
import com.folib.db.schema.Vertices;
import com.folib.domain.GenericCoordinatesEntity;
import com.folib.gremlin.dsl.EntityTraversal;
import com.folib.gremlin.dsl.__;
import org.apache.tinkerpop.gremlin.process.traversal.Traverser;
import org.apache.tinkerpop.gremlin.structure.Element;
import org.apache.tinkerpop.gremlin.structure.Vertex;
import org.springframework.stereotype.Component;

import java.util.Map;
import java.util.Map.Entry;

import static com.folib.gremlin.dsl.EntityTraversalUtils.extractObject;
import static com.folib.gremlin.dsl.EntityTraversalUtils.extractPropertyList;
import static org.apache.tinkerpop.gremlin.structure.VertexProperty.Cardinality.single;

/**
 * @author xuxinping
 */
@Component
public class GenericCoordinatesAdapter
        implements VertexEntityTraversalAdapter<GenericCoordinates>, ArtifactCoodrinatesNodeAdapter
{

    @Override
    public String label()
    {
        return Vertices.GENERIC_COORDINATES;
    }

    @Override
    public Class<? extends GenericCoordinates> entityClass()
    {
        return GenericCoordinates.class;
    }

    @Override
    public EntityTraversal<Vertex, GenericCoordinates> fold()
    {
        return __.<Vertex, Object>project("id", "uuid", "version", "coordinates")
                 .by(__.id())
                 .by(__.enrichPropertyValue("uuid"))
                 .by(__.enrichPropertyValue("version"))
                 .by(__.propertyMap())
                 .map(this::map);
    }

    private GenericCoordinates map(Traverser<Map<String, Object>> t)
    {
        GenericCoordinatesEntity result = new GenericCoordinatesEntity();
        result.setNativeId(extractObject(Long.class, t.get().get("id")));
        result.setUuid(extractObject(String.class, t.get().get("uuid")));
        result.setVersion(extractObject(String.class, t.get().get("version")));

        Map<String, Object> coordinates = (Map<String, Object>) t.get().get("coordinates");
        coordinates.remove("uuid");
        coordinates.remove("version");
        coordinates.remove("created");
        coordinates.entrySet()
                   .stream()
                   .forEach(e -> result.setCoordinate(e.getKey().replace("coordinates.", ""),
                                                      extractPropertyList(String.class, e.getValue()).iterator().next()));

        return result;
    }

    @Override
    public UnfoldEntityTraversal<Vertex, Vertex> unfold(GenericCoordinates entity)
    {
        EntityTraversal<Vertex, Vertex> t = __.<Vertex>identity();

        if (entity.getVersion() != null)
        {
            t = t.property(single, "version", entity.getVersion());
        }

        for (Entry<String, String> coordinateEntry : entity.getCoordinates().entrySet())
        {
            if (coordinateEntry.getValue() == null)
            {
                continue;
            }
            t = t.property(single, "coordinates." + coordinateEntry.getKey(), coordinateEntry.getValue());
        }

        return new UnfoldEntityTraversal<>(Vertices.GENERIC_COORDINATES, entity, t);
    }

    @Override
    public EntityTraversal<Vertex, Element> cascade()
    {
        throw new UnsupportedOperationException();
    }

}
