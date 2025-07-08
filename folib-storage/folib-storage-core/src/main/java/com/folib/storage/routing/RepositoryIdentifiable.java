package com.folib.storage.routing;

/**
 * @author veadan
 */
public interface RepositoryIdentifiable
{
    String getStorageId();

    String getRepositoryId();

    String getStorageIdAndRepositoryId();
}
