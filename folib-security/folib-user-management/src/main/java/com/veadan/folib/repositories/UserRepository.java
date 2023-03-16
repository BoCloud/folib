package com.veadan.folib.repositories;

import com.veadan.folib.db.schema.Edges;
import com.veadan.folib.db.schema.Properties;
import com.veadan.folib.db.schema.Vertices;
import com.veadan.folib.domain.User;
import com.veadan.folib.gremlin.adapters.EntityTraversalAdapter;
import com.veadan.folib.gremlin.adapters.UserAdapter;
import com.veadan.folib.gremlin.repositories.GremlinVertexRepository;
import org.apache.tinkerpop.gremlin.process.traversal.P;
import org.apache.tinkerpop.gremlin.structure.Vertex;
import org.springframework.data.neo4j.annotation.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import javax.inject.Inject;
import javax.transaction.Transactional;
import java.util.List;

@Repository
@Transactional
public class UserRepository extends GremlinVertexRepository<User>
        implements UserQueries {


    @Inject
    UserQueries queries;

    @Inject
    UserAdapter adapter;

    @Override
    protected EntityTraversalAdapter<Vertex, User> adapter() {
        return adapter;
    }

    @Override
    public List<User> findUsersWithRole(String role) {
        return g().V().hasLabel(Vertices.SECURITY_ROLE).has(Properties.UUID, role).inE(Edges.USER_HAS_SECURITY_ROLES).outV()
                .has(Properties.USER_TYPE, "general").has(Properties.ENABLED, true).map(adapter.fold()).dedup().toList();
    }

    public List<User> findUsersWithRoles(List<String> roleList) {
        return g().V().hasLabel(Vertices.SECURITY_ROLE).has(Properties.UUID, P.within(roleList)).inE(Edges.USER_HAS_SECURITY_ROLES).outV()
                .has(Properties.USER_TYPE, "general").has(Properties.ENABLED, true).map(adapter.fold()).dedup().toList();
    }

    @Override
    public Iterable<User> findAll() {
        return g().V().hasLabel(Vertices.USER).has(Properties.USER_TYPE, "general").map(adapter.fold()).toList();
    }

    @Override
    public List<User> findAllUsers() {
        return g().V().hasLabel(Vertices.USER).has(Properties.USER_TYPE, "general").has(Properties.ENABLED, true).map(adapter.fold()).toList();
    }

}

@Repository
interface UserQueries extends org.springframework.data.repository.Repository<User, String> {

    @Query("MATCH (user:User)-[r]->(securityRole:SecurityRole) " +
            "WHERE securityRole.uuid=$role " +
            "RETURN user, r, securityRole")
    List<User> findUsersWithRole(@Param("role") String role);

    @Query("MATCH (user:User)-[r]->(securityRole:SecurityRole) " +
            "WHERE user.userType='general' AND user.enabled='true'" +
            "RETURN user, r, securityRole")
    List<User> findAllUsers();

}
