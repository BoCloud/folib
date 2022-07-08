package com.veadan.folib.services;

import java.util.Set;

import com.veadan.folib.artifact.coordinates.ArtifactCoordinates;
import com.veadan.folib.domain.Artifact;
import com.veadan.folib.domain.ArtifactGroup;
import com.veadan.folib.storage.repository.Repository;

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
