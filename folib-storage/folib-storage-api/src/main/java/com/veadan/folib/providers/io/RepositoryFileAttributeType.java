package com.veadan.folib.providers.io;

import java.util.Arrays;

public enum RepositoryFileAttributeType
{

    COORDINATES("coordinates"),

    METADATA("metadata"),

    CHECKSUM("checksum"),

    TRASH("trash"),

    TEMP("temp"),

    EXPIRED("expired"),
    
    ARTIFACT("artifact"),
    
    ARTIFACT_PATH("artifactPath"),

    RESOURCE_URL("resourceUrl"),
    
    REPOSITORY_ID("repositoryId"),
    
    STORAGE_ID("storageId");

    private String name;

    private RepositoryFileAttributeType(String name)
    {
        this.name = name;
    }

    public String getName()
    {
        return name;
    }

    public static RepositoryFileAttributeType of(String s)
    {
        return Arrays.stream(values())
                     .filter(e -> e.getName().equals(s))
                     .findFirst()
                     .orElseThrow(() -> new IllegalArgumentException(s));
    }

}
