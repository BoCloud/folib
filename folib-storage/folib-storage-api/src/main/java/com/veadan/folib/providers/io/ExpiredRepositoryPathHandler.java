package com.veadan.folib.providers.io;

import java.io.IOException;

/**
 * @author veadan
 */
public interface ExpiredRepositoryPathHandler
{

    default boolean supports(RepositoryPath repositoryPath)
            throws IOException
    {
        return true;
    }

    void handleExpiration(RepositoryPath repositoryPath)
            throws IOException;

}
