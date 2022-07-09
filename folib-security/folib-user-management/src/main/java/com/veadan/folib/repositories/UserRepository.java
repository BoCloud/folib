package com.veadan.folib.repositories;

import com.veadan.folib.db.schema.Edges;
import com.veadan.folib.db.schema.Vertices;
import com.veadan.folib.domain.User;
import com.veadan.folib.gremlin.adapters.EntityTraversalAdapter;
import com.veadan.folib.gremlin.adapters.UserAdapter;
import com.veadan.folib.gremlin.dsl.EntityTraversal;
import com.veadan.folib.gremlin.repositories.GremlinVertexRepository;

import javax.inject.Inject;
import javax.inject.Named;
import javax.transaction.Transactional;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import org.apache.tinkerpop.gremlin.structure.Vertex;
import org.json.simple.JSONArray;
import org.springframework.data.neo4j.annotation.Depth;
import org.springframework.data.neo4j.annotation.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;
import org.springframework.util.StopWatch;

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

//        StopWatch sw = new StopWatch();
//        sw.start("第1");
//        EntityTraversal<Vertex, User> a=g().V().hasLabel(Vertices.USER)
//                .inE(Edges.USER_HAS_SECURITY_ROLES).hasLabel(Vertices.SECURITY_ROLE).map(adapter.fold());
//
//        System.out.println(a.toList());
//        List<User> list= new ArrayList<>();
//        if(a.hasNext()){
//            list.add(a.next());
//        }
//
//
//        sw.stop();
//        List<User> users= queries.findUsersWithRole("ADMIN");

//        System.out.println(sw.prettyPrint());
        return queries.findAllUsers();
    }

}

@Repository
interface UserQueries extends org.springframework.data.repository.Repository<User, String>
{

    @Query("MATCH (user:User)-[r]->(securityRole:SecurityRole) " +
           "WHERE securityRole.uuid=$role " +
           "RETURN user, r, securityRole")
    List<User> findUsersWithRole(@Param("role") String role);

    @Query("MATCH (user:User)-[r]->(securityRole:SecurityRole) " +
           "RETURN user, r, securityRole")
    List<User> findAllUsers();

}
