package com.veadan.folib.repositories;

import com.veadan.folib.domain.User;
import com.veadan.folib.gremlin.adapters.EntityTraversalAdapter;
import com.veadan.folib.gremlin.adapters.UserAdapter;
import com.veadan.folib.gremlin.repositories.GremlinVertexRepository;

import javax.inject.Inject;
import javax.transaction.Transactional;

import java.util.List;

import org.apache.tinkerpop.gremlin.structure.Vertex;
import org.springframework.data.neo4j.annotation.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

@Repository
@Transactional
public class UserRepository extends GremlinVertexRepository<User>
        implements UserQueries
{

    @Inject
    UserQueries queries;

    @Inject
    UserAdapter adapter;

    @Override
    protected EntityTraversalAdapter<Vertex, User> adapter()
    {
        return adapter;
    }

    public List<User> findUsersWithRole(String role)
    {
        return queries.findUsersWithRole(role);
    }

    @Override
    public Iterable<User> findAll()
    {
        return findAllUsers();
    }

    @Override
    public List<User> findAllUsers()
    {
        return queries.findAllUsers();
    }

}

@Repository
interface UserQueries
        extends org.springframework.data.repository.Repository<User, String>
{

    @Query("MATCH (user:User)-[r]->(securityRole:SecurityRole) " +
           "WHERE securityRole.uuid=$role " +
           "RETURN user, r, securityRole")
    List<User> findUsersWithRole(@Param("role") String role);

    @Query("MATCH (user:User)-[r]->(securityRole:SecurityRole) " +
           "RETURN user, r, securityRole")
    List<User> findAllUsers();

}
