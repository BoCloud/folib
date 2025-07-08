package com.folib.repositories;

import jakarta.inject.Inject;

import com.folib.gremlin.adapters.ArtifactTagAdapter;
import com.folib.artifact.ArtifactTag;
import com.folib.gremlin.repositories.GremlinVertexRepository;
import jakarta.transaction.Transactional;
import org.springframework.stereotype.Repository;

/**
 * @author veadan
 */
@Repository
@Transactional
public class ArtifactTagRepository extends GremlinVertexRepository<ArtifactTag>
      //  implements ArtifactTagQueries
{

    @Inject
    ArtifactTagAdapter adapter;
    
    //@Inject
    //@Lazy
    //ArtifactTagQueries queries;

    @Override
    protected ArtifactTagAdapter adapter()
    {
        return adapter;
    }

}

//@Repository
//interface ArtifactTagQueries
//        extends org.springframework.data.repository.Repository<ArtifactTag, String>
//{
//
//}
