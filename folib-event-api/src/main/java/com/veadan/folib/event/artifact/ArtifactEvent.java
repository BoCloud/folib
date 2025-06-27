package com.veadan.folib.event.artifact;

import java.nio.file.Path;

import com.veadan.folib.event.RepositoryBasedEvent;

/**
 * @author veadan
 */
public class ArtifactEvent<T extends Path> extends RepositoryBasedEvent<T>
{

    private T targetPath;

    public ArtifactEvent(T sourcePath,
                         int type)
    {
        super(sourcePath, type);
    }

    public ArtifactEvent(T sourcePath,
                         T targetPath,
                         int type)
    {
        super(sourcePath, type);
        this.targetPath = targetPath;
    }

    public T getTargetPath()
    {
        return targetPath;
    }

    public void setTargetPath(T targetPath)
    {
        this.targetPath = targetPath;
    }

}
