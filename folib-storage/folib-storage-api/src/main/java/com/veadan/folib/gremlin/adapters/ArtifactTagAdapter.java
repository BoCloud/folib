package com.veadan.folib.gremlin.adapters;

import static com.veadan.folib.gremlin.dsl.EntityTraversalUtils.extractObject;

import java.util.Map;

import com.veadan.folib.gremlin.dsl.EntityTraversal;
import com.veadan.folib.gremlin.dsl.__;
import org.apache.tinkerpop.gremlin.process.traversal.Traverser;
import org.apache.tinkerpop.gremlin.structure.Element;
import org.apache.tinkerpop.gremlin.structure.Vertex;
import com.veadan.folib.artifact.ArtifactTag;
import com.veadan.folib.db.schema.Vertices;
import com.veadan.folib.domain.ArtifactTagEntity;
import org.springframework.stereotype.Component;

/**
 * @author veadan
 */
@Component
public class ArtifactTagAdapter implements VertexEntityTraversalAdapter<ArtifactTag>
{

    @Override
    public String label()
    {
        return Vertices.ARTIFACT_TAG;
    }

    @Override
    public EntityTraversal<Vertex, ArtifactTag> fold()
    {
        return __.<Vertex, Object>project("id", "uuid")
                 .by(__.id())
                 .by(__.enrichPropertyValue("uuid"))
                 .map(this::map);
    }

    private ArtifactTag map(Traverser<Map<String, Object>> t)
    {
        ArtifactTagEntity result = new ArtifactTagEntity();
        result.setNativeId(extractObject(Long.class, t.get().get("id")));
        result.setUuid(extractObject(String.class, t.get().get("uuid")));

        return result;
    }

    @Override
    public UnfoldEntityTraversal<Vertex, Vertex> unfold(ArtifactTag entity)
    {
        return new UnfoldEntityTraversal<>(Vertices.ARTIFACT_TAG, entity, __.identity());
    }

    @Override
    public EntityTraversal<Vertex, Element> cascade()
    {
        return __.<Vertex>identity().map(t -> Element.class.cast(t.get()));
    }

}
