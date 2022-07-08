package com.veadan.folib.domain;

import com.veadan.folib.artifact.MavenArtifactUtils;
import org.apache.maven.index.artifact.Gav;

/**
 * @author veadan
 */
public class MavenArtifactEntryUtils
{

    public static Gav toGav(Artifact artifactEntry)
    {
        return MavenArtifactUtils.convertPathToGav(artifactEntry.getArtifactPath());
    }

}
