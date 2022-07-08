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

}
