package com.veadan.folib.repositories;

import com.veadan.folib.db.schema.Edges;
import com.veadan.folib.db.schema.Properties;
import com.veadan.folib.db.schema.Vertices;
import com.veadan.folib.domain.SecurityRole;
import com.veadan.folib.domain.User;
import com.veadan.folib.gremlin.adapters.EntityTraversalAdapter;
import com.veadan.folib.gremlin.adapters.UserAdapter;
import com.veadan.folib.gremlin.dsl.EntityTraversal;
import com.veadan.folib.gremlin.repositories.GremlinVertexRepository;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.tinkerpop.gremlin.process.traversal.P;
import org.apache.tinkerpop.gremlin.structure.Vertex;
import org.janusgraph.core.attribute.Text;
import org.springframework.stereotype.Repository;

import javax.inject.Inject;
import javax.transaction.Transactional;
import java.util.List;
import java.util.stream.Collectors;

@Repository
@Transactional
public class UserRepository extends GremlinVertexRepository<User> {

    @Inject
    UserAdapter adapter;

    @Override
    protected EntityTraversalAdapter<Vertex, User> adapter() {
        return adapter;
    }

    public List<User> findUsersWithRole(String role) {
        return g().V().hasLabel(Vertices.SECURITY_ROLE).has(Properties.UUID, role).inE(Edges.USER_HAS_SECURITY_ROLES).outV()
                .has(Properties.USER_TYPE, "general").has(Properties.CREATED, P.gt(0)).has(Properties.ENABLED, true).map(adapter.fold()).dedup().toList();
    }

    public List<User> findUsersWithRoles(List<String> roleList) {
        return g().V().hasLabel(Vertices.SECURITY_ROLE).has(Properties.UUID, P.within(roleList)).inE(Edges.USER_HAS_SECURITY_ROLES).outV()
                .has(Properties.USER_TYPE, "general").has(Properties.CREATED, P.gt(0)).has(Properties.ENABLED, true).map(adapter.fold()).dedup().toList();
    }

    @Override
    public Iterable<User> findAll() {
        return g().V().hasLabel(Vertices.USER).has(Properties.USER_TYPE, "general").has(Properties.CREATED, P.gt(0)).map(adapter.fold()).toList();
    }

    public List<User> findUsersPage(User user, int start, int end) {
        return commonUserPage(user).range(start, end).map(adapter.fold()).dedup().toList();
    }

    public Long countUsers(User user) {
        return commonUserPage(user).count().tryNext().orElse(0L);
    }

    private EntityTraversal<Vertex, Vertex> commonUserPage(User user) {
        EntityTraversal<Vertex, Vertex> entityTraversal;
        if (CollectionUtils.isNotEmpty(user.getRoles())) {
            entityTraversal = g().V().hasLabel(Vertices.SECURITY_ROLE).has(Properties.UUID, P.within(user.getRoles().stream().map(SecurityRole::getRoleName).collect(Collectors.toList()))).inE(Edges.USER_HAS_SECURITY_ROLES).outV()
                    .has(Properties.USER_TYPE, "general").has(Properties.CREATED, P.gt(0));
        } else {
            entityTraversal = g().V().hasLabel(Vertices.USER).has(Properties.USER_TYPE, "general").has(Properties.CREATED, P.gt(0));
        }
        if (StringUtils.isNotBlank(user.getUsername())) {
            entityTraversal = entityTraversal.has(Properties.UUID, Text.textContains(user.getUsername()));
        }
        if (StringUtils.isNotBlank(user.getEmail())) {
            entityTraversal = entityTraversal.has(Properties.EMAIL, Text.textContains(user.getEmail()));
        }
        return entityTraversal;
    }

}
