package com.folib.services.impl;

import java.lang.reflect.UndeclaredThrowableException;
import java.util.Optional;

import jakarta.inject.Inject;

import org.apache.tinkerpop.gremlin.structure.Graph;
import com.folib.artifact.ArtifactTag;
import com.folib.domain.ArtifactTagEntity;
import com.folib.gremlin.dsl.EntityTraversalSource;
import com.folib.repositories.ArtifactTagRepository;
import com.folib.services.ArtifactTagService;
import org.janusgraph.core.JanusGraph;
import org.springframework.cache.annotation.Cacheable;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Service
@Transactional
public class ArtifactTagServiceImpl implements ArtifactTagService
{

    @Inject
    private ArtifactTagRepository artifactTagRepository;
    @Inject
    private JanusGraph janusGraph;

    @Override
    @Cacheable(value = "tags", key = "#name != null ? #name : 'default'")
    public ArtifactTag findOneOrCreate(String name)
    {
        Optional<ArtifactTag> optionalResult = artifactTagRepository.findById(name);

        return optionalResult.orElseGet(() -> {
            ArtifactTagEntity artifactTagEntry = new ArtifactTagEntity();
            artifactTagEntry.setName(name);

            Graph g = janusGraph.tx().createThreadedTx();
            try
            {
                ArtifactTagEntity result = artifactTagRepository.save(() -> g.traversal(EntityTraversalSource.class), artifactTagEntry);
                g.tx().commit();

                return result;
            }
            catch (Exception e)
            {
                g.tx().rollback();
                throw new UndeclaredThrowableException(e);
            }
            finally
            {
                g.tx().close();
            }
        });
    }

}
