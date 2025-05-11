package com.veadan.folib.repositories;

import javax.inject.Inject;

import com.veadan.folib.gremlin.adapters.ArtifactTagAdapter;
import com.veadan.folib.artifact.ArtifactTag;
import com.veadan.folib.gremlin.repositories.GremlinVertexRepository;
import jakarta.transaction.Transactional;
import org.springframework.context.annotation.Lazy;
import org.springframework.stereotype.Repository;

/**
 * @author xuxinping
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
