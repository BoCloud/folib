package com.veadan.folib.web;

/**
 * @author veadan
 */
public final class Constants
{

    private Constants()
    {
    }

    public static final String ARTIFACT_ROOT_PATH = "/storages";

    public static final String REPOSITORY_REQUEST_ATTRIBUTE = Constants.class.getName() + ".repository";

    public static final String STORAGE_NOT_FOUND_REQUEST_ATTRIBUTE = Constants.class.getName() + ".storageNotFound";

    public static final String REPOSITORY_NOT_FOUND_REQUEST_ATTRIBUTE =
            Constants.class.getName() + ".repositoryNotFound";

    public static final String DOCKER_ROOT_PATH = "/v2";

    public static final String STORAGE_ROOT_PATH = "/api/configuration/folib/storages";

}
