package com.folib.services;

import java.util.Set;

import com.folib.artifact.coordinates.ArtifactCoordinates;
import com.folib.domain.Artifact;
import com.folib.domain.ArtifactGroup;
import com.folib.storage.repository.Repository;

/**
 * @author veadan
 */
public interface ArtifactGroupService<T extends ArtifactGroup>
{

    void saveArtifacts(Repository repository,
                       Set<Artifact> artifactToSaveSet);

    ArtifactCoordinates addArtifactToGroup(T artifactGroup,
                                           Artifact artifactEntry);

}
