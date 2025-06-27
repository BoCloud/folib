package com.veadan.folib.artifact.locator.handlers;

import java.io.IOException;
import java.util.LinkedHashMap;
import java.util.List;

import com.veadan.folib.providers.io.RepositoryPath;

/**
 * @author veadan
 * @author stodorov
 */
public interface ArtifactDirectoryOperation
{

    /**
     * Operation logic which need to be performed on provided directory.
     * 
     * @param directoryPath
     * @throws IOException
     */
    void execute(RepositoryPath directoryPath) throws IOException;

    LinkedHashMap<RepositoryPath, List<RepositoryPath>> getVisitedRootPaths();

    RepositoryPath getBasePath();
    
}
