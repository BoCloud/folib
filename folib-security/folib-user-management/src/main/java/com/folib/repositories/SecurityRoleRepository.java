package com.folib.repositories;

import com.folib.domain.SecurityRole;
import com.folib.gremlin.adapters.EntityTraversalAdapter;
import com.folib.gremlin.adapters.SecurityRoleAdapter;
import com.folib.gremlin.repositories.GremlinVertexRepository;

import javax.inject.Inject;

import jakarta.transaction.Transactional;
import org.apache.tinkerpop.gremlin.structure.Vertex;
import org.springframework.stereotype.Repository;

/**
 * @author veadan
 */
@Repository
@Transactional
public class SecurityRoleRepository extends GremlinVertexRepository<SecurityRole>
        //implements SecurityRoleQueries
{
    //@Lazy
    //@Inject
    //private SecurityRoleQueries queries;

    @Inject
    private SecurityRoleAdapter roleAdapter;

    //@Override
    //public List<SecurityRole> findAllUserRoles()
    //{
    //    return queries.findAllUserRoles();
    //}

    @Override
    protected EntityTraversalAdapter<Vertex, SecurityRole> adapter()
    {
        return roleAdapter;
    }

}

//@Repository
//interface SecurityRoleQueries
//        extends org.springframework.data.repository.Repository<SecurityRole, String>
//{
//
//    @Query("MATCH (securityRole:SecurityRole) " +
//           "RETURN securityRole")
//    List<SecurityRole> findAllUserRoles();
//
//}
