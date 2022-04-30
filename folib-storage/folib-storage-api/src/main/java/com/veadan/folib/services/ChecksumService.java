package com.veadan.folib.services;

import java.io.IOException;

/**
 * @author Kate Novik.
 */
public interface ChecksumService
{

    /**
     * Regenerate checksum for artifact using artifactPath (string)
     *
     * @param storageId         String
     * @param repositoryId      String
     * @param basePath      String
     * @param forceRegeneration boolean
     */
    void regenerateChecksum(String storageId,
                            String repositoryId,
                            String basePath,
                            boolean forceRegeneration)
            throws IOException;

}
