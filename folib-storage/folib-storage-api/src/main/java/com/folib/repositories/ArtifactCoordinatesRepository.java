package com.folib.repositories;

import jakarta.inject.Inject;

import com.folib.artifact.coordinates.ArtifactCoordinates;
import com.folib.artifact.coordinates.GenericCoordinates;
import com.folib.gremlin.adapters.ArtifactCoordinatesHierarchyAdapter;
import com.folib.gremlin.repositories.GremlinVertexRepository;
import jakarta.transaction.Transactional;
import org.springframework.stereotype.Repository;

@Repository
@Transactional
public class ArtifactCoordinatesRepository extends GremlinVertexRepository<GenericCoordinates>
        //implements ArtifactCoordinatesQueries
{

    @Inject
    ArtifactCoordinatesHierarchyAdapter artifactCoordinatesAdapter;
    //@Lazy
    //@Inject
    //ArtifactCoordinatesQueries queries;

    @Override
    protected ArtifactCoordinatesHierarchyAdapter adapter()
    {
        return artifactCoordinatesAdapter;
    }

    @Override
    public <R extends GenericCoordinates> R save(R entity)
    {
        if (entity.getUuid() == null)
        {
            ((ArtifactCoordinates)entity).buildPath();
        }

        return super.save(entity);
    }

}

//@Repository
//interface ArtifactCoordinatesQueries extends org.springframework.data.repository.Repository<GenericArtifactCoordinates, String>
//{
//
//}
