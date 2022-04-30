package com.veadan.folib.gremlin.adapters;

import static org.apache.tinkerpop.gremlin.structure.VertexProperty.Cardinality.single;
import static com.veadan.folib.gremlin.dsl.EntityTraversalUtils.extractObject;
import static com.veadan.folib.gremlin.dsl.EntityTraversalUtils.toLocalDateTime;
import static com.veadan.folib.gremlin.dsl.EntityTraversalUtils.toLong;

import com.veadan.folib.gremlin.dsl.EntityTraversal;
import com.veadan.folib.gremlin.dsl.__;
import com.veadan.folib.db.schema.Edges;
import com.veadan.folib.db.schema.Vertices;
import com.veadan.folib.domain.User;
import com.veadan.folib.domain.UserEntity;
import com.veadan.folib.domain.SecurityRole;
import com.veadan.folib.gremlin.dsl.EntityTraversalUtils;

import javax.inject.Inject;

import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.UUID;

import org.apache.tinkerpop.gremlin.process.traversal.Traverser;
import org.apache.tinkerpop.gremlin.structure.Element;
import org.apache.tinkerpop.gremlin.structure.Vertex;
import org.springframework.stereotype.Component;

/**
 * @author sbespalov
 */
@Component
public class UserAdapter implements VertexEntityTraversalAdapter<User>
{

    @Inject
    private SecurityRoleAdapter securityRoleAdapter;

    @Override
    public String label()
    {
        return Vertices.USER;
    }

    @Override
    public EntityTraversal<Vertex, User> fold()
    {
        return __.<Vertex, Object>project("id",
                                          "uuid",
                                          "password",
                                          "enabled",
                                          "roles",
                                          "securityTokenKey",
                                          "lastUpdated",
                                          "sourceId")
                 .by(__.id())
                 .by(__.enrichPropertyValue("uuid"))
                 .by(__.enrichPropertyValue("password"))
                 .by(__.enrichPropertyValue("enabled"))
                 .by(__.outE(Edges.USER_HAS_SECURITY_ROLES)
                       .inV()
                       .map(securityRoleAdapter.fold())
                       .map(EntityTraversalUtils::castToObject)
                       .fold())
                 .by(__.enrichPropertyValue("securityTokenKey"))
                 .by(__.enrichPropertyValue("lastUpdated"))
                 .by(__.enrichPropertyValue("sourceId"))
                 .map(this::map);
    }

    private User map(Traverser<Map<String, Object>> t)
    {
        UserEntity result = new UserEntity(extractObject(String.class, t.get().get("uuid")));
        result.setNativeId(extractObject(Long.class, t.get().get("id")));

        result.setPassword(extractObject(String.class, t.get().get("password")));
        result.setEnabled(extractObject(Boolean.class, t.get().get("enabled")));
        List<SecurityRole> userRoles = (List<SecurityRole>) t.get().get("roles");
        result.setRoles(new HashSet<>(userRoles));
        result.setSecurityTokenKey(extractObject(String.class, t.get().get("securityTokenKey")));
        result.setLastUpdated(toLocalDateTime(extractObject(Long.class, t.get().get("lastUpdated"))));
        result.setSourceId(extractObject(String.class, t.get().get("sourceId")));

        return result;
    }

    @Override
    public UnfoldEntityTraversal<Vertex, Vertex> unfold(User entity)
    {
        String storedUserId = Vertices.USER + ":" + UUID.randomUUID().toString();

        EntityTraversal<Vertex, Vertex> userRoleTraversal = __.identity();
        EntityTraversal<Vertex, Vertex> unfoldTraversal = __.identity();

        unfoldTraversal.sideEffect(__.outE(Edges.USER_HAS_SECURITY_ROLES).drop());

        for (SecurityRole securityRole : entity.getRoles())
        {
            userRoleTraversal = userRoleTraversal.V(securityRole)
                                                 .saveV(securityRole.getUuid(),
                                                        securityRoleAdapter.unfold(securityRole));

            userRoleTraversal = userRoleTraversal.addE(Edges.USER_HAS_SECURITY_ROLES)
                                                 .from(__.<Vertex, Vertex>select(storedUserId).unfold())
                                                 .inV();

            userRoleTraversal = userRoleTraversal.inE(Edges.USER_HAS_SECURITY_ROLES).outV();

        }

        unfoldTraversal = unfoldTraversal.map(unfoldUser(entity))
                                         .store(storedUserId)
                                         .map(userRoleTraversal);

        return new UnfoldEntityTraversal<>(Vertices.USER, entity, unfoldTraversal);
    }

    private EntityTraversal<Vertex, Vertex> unfoldUser(User entity)
    {
        EntityTraversal<Vertex, Vertex> t = __.<Vertex>identity();

        if (entity.getPassword() != null)
        {
            t = t.property(single, "password", entity.getPassword());
        }
        if (entity.getSecurityTokenKey() != null)
        {
            t = t.property(single, "securityTokenKey", entity.getSecurityTokenKey());
        }
        if (entity.getSourceId() != null)
        {
            t = t.property(single, "sourceId", entity.getSourceId());
        }
        if (entity.getLastUpdated() != null)
        {
            t = t.property(single, "lastUpdated", toLong(entity.getLastUpdated()));
        }

        t = t.property(single, "enabled", entity.isEnabled());

        return t;
    }

    @Override
    public EntityTraversal<Vertex, Element> cascade()
    {
        return __.<Vertex>identity().map(t -> Element.class.cast(t.get()));
    }

}
