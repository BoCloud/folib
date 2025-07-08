package com.folib.providers.io;

import com.folib.domain.Artifact;

import java.io.IOException;
import java.nio.file.Files;

/**
 * The main concept of {@link TempRepositoryPath} is to provide atomacity into
 * artifact files store process. Files stored into temporary location first,
 * along with all additional logic needed, and, if procedure completed
 * successfully, then file just moved into original location, other way
 * "transaction" will be rolled back and temporary file will be removed.
 *
 * @see StorageFileSystemProvider.TempOutputStream
 * @author veadan
 *
 */
public class TempRepositoryPath extends RepositoryPath
{

    private RepositoryPath tempTarget;

    private TempRepositoryPath(RepositoryPath tempPath)
    {
        super(tempPath.getTarget(), tempPath.getFileSystem());
    }

    public RepositoryPath getTempTarget()
    {
        return tempTarget;
    }

    public static TempRepositoryPath of(RepositoryPath path)
        throws IOException
    {
        RepositoryPath tempPathBase = path.getFileSystem().getTempPath();
        RepositoryPath tempPath = StorageFileSystemProvider.rebase(path, tempPathBase);

        if (!Files.exists(tempPath.getParent().getTarget()))
        {
            Files.createDirectories(tempPath.getParent().getTarget());
        }

        TempRepositoryPath result = new TempRepositoryPath(tempPath);
        result.tempTarget = path;
        result.artifact = path.artifact;

        return result;
    }
    
    @Override
    public Artifact getArtifactEntry()
        throws IOException
    {
        return tempTarget.getArtifactEntry();
    }

    @Override
    public Boolean getArtifactExist() throws IOException
    {
        return tempTarget.getArtifactExist();
    }
}
