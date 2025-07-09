package com.folib.gremlin.adapters;

import static com.folib.gremlin.dsl.EntityTraversalUtils.extractObject;

import com.folib.gremlin.dsl.EntityTraversal;
import com.folib.gremlin.dsl.__;
import com.folib.db.schema.Vertices;
import com.folib.domain.SecurityRole;
import com.folib.domain.SecurityRoleEntity;

import java.util.Map;

import org.apache.tinkerpop.gremlin.process.traversal.Traverser;
import org.apache.tinkerpop.gremlin.structure.Element;
import org.apache.tinkerpop.gremlin.structure.Vertex;
import org.springframework.stereotype.Component;

/**
 * @author veadan
 */
@Component
public class SecurityRoleAdapter implements VertexEntityTraversalAdapter<SecurityRole>
{

    @Override
    public String label()
    {
        return Vertices.SECURITY_ROLE;
    }

    @Override
    public EntityTraversal<Vertex, SecurityRole> fold()
    {
        return __.<Vertex, Object>project("id", "uuid")
                 .by(__.id())
                 .by(__.enrichPropertyValue("uuid"))
                 .map(this::map);
    }

    private SecurityRole map(Traverser<Map<String, Object>> t)
    {
        SecurityRoleEntity result = new SecurityRoleEntity();
        result.setNativeId(extractObject(Long.class, t.get().get("id")));
        result.setUuid(extractObject(String.class, t.get().get("uuid")));

        return result;
    }

    @Override
    public UnfoldEntityTraversal<Vertex, Vertex> unfold(SecurityRole entity)
    {
        return new UnfoldEntityTraversal<>(Vertices.SECURITY_ROLE, entity, __.identity());
    }

    @Override
    public EntityTraversal<Vertex, Element> cascade()
    {
        return __.<Vertex>identity().map(t -> Element.class.cast(t.get()));
    }

}
