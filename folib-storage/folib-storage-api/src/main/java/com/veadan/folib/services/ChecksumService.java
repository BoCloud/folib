package com.veadan.folib.services;

import java.io.IOException;

/**
 * @author veadan
 */
public interface ChecksumService
{

    /**
     * Regenerate checksum for artifact using artifactPath (string)
     *
     * @param storageId         String
     * @param repositoryId      String
     * @param basePath      String
     * @param lastModifiedTime String
     * @param forceRegeneration boolean
     */
    void regenerateChecksum(String storageId,
                            String repositoryId,
                            String basePath,
                            String lastModifiedTime,
                            boolean forceRegeneration)
            throws IOException;

}
