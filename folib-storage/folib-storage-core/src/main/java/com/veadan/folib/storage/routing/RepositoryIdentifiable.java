package com.veadan.folib.storage.routing;

/**
 * @author Przemyslaw Fusik
 */
public interface RepositoryIdentifiable
{
    String getStorageId();

    String getRepositoryId();

    String getStorageIdAndRepositoryId();
}
