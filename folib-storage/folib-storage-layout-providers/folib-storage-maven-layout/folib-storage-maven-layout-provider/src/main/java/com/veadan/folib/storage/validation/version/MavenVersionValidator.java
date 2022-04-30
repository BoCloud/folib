package com.veadan.folib.storage.validation.version;

import com.veadan.folib.storage.validation.MavenArtifactCoordinatesValidator;

import org.apache.maven.artifact.ArtifactUtils;

/**
 * @author Przemyslaw Fusik
 * @author carlspring
 */
interface MavenVersionValidator
        extends MavenArtifactCoordinatesValidator
{

    default boolean isRelease(String version)
    {
        return version != null && !isSnapshot(version);
    }

    default boolean isSnapshot(String version)
    {
        return version != null && ArtifactUtils.isSnapshot(version);
    }

}
