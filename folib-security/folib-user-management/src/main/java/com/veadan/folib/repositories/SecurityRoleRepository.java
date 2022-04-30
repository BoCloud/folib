package com.veadan.folib.repositories;

import com.veadan.folib.domain.SecurityRole;
import com.veadan.folib.gremlin.adapters.EntityTraversalAdapter;
import com.veadan.folib.gremlin.adapters.SecurityRoleAdapter;
import com.veadan.folib.gremlin.repositories.GremlinVertexRepository;

import javax.inject.Inject;
import javax.transaction.Transactional;

import java.util.List;

import org.apache.tinkerpop.gremlin.structure.Vertex;
import org.springframework.data.neo4j.annotation.Query;
import org.springframework.stereotype.Repository;

/**
 * @author ankit.tomar
 */
@Repository
@Transactional
public class SecurityRoleRepository extends GremlinVertexRepository<SecurityRole>
        implements SecurityRoleQueries
{
    @Inject
    private SecurityRoleQueries queries;

    @Inject
    private SecurityRoleAdapter roleAdapter;

    @Override
    public List<SecurityRole> findAllUserRoles()
    {
        return queries.findAllUserRoles();
    }

    @Override
    protected EntityTraversalAdapter<Vertex, SecurityRole> adapter()
    {
        return roleAdapter;
    }

}

@Repository
interface SecurityRoleQueries
        extends org.springframework.data.repository.Repository<SecurityRole, String>
{

    @Query("MATCH (securityRole:SecurityRole) " +
           "RETURN securityRole")
    List<SecurityRole> findAllUserRoles();

}
