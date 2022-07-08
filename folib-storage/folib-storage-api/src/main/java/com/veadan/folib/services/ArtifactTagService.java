package com.veadan.folib.services;

import com.veadan.folib.artifact.ArtifactTag;

/**
 * @author @author veadan
 *
 */
public interface ArtifactTagService
{

    ArtifactTag findOneOrCreate(String name);

}
