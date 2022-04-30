package com.veadan.folib.services;

import com.veadan.folib.artifact.ArtifactTag;

/**
 * @author Sergey Bespalov
 *
 */
public interface ArtifactTagService
{

    ArtifactTag findOneOrCreate(String name);

}
