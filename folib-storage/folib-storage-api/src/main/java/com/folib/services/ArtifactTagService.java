package com.folib.services;

import com.folib.artifact.ArtifactTag;

/**
 * @author @author veadan
 *
 */
public interface ArtifactTagService
{

    ArtifactTag findOneOrCreate(String name);

}
