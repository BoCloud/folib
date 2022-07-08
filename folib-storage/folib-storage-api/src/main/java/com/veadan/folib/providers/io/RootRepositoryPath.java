package com.veadan.folib.providers.io;

import java.nio.file.Path;

import com.veadan.folib.domain.Artifact;

/**
 * @author xuxinping
 *
 */
public class RootRepositoryPath extends RepositoryPath
{

    public RootRepositoryPath(Path target,
                              LayoutFileSystem fileSystem)
    {
        super(target, fileSystem);
    }

    public RepositoryPath resolve(Artifact artifactEntry)
    {
        RepositoryPath result = super.resolve(artifactEntry.getArtifactPath());
        result.artifact = artifactEntry;
        return result;
    }

}
