package com.veadan.folib.providers.io;

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

public enum RepositoryFileAttributeType {

    COORDINATES("coordinates"),

    METADATA("metadata"),

    CHECKSUM("checksum"),

    TEMP("temp"),

    EXPIRED("expired"),

    ARTIFACT("artifact"),

    ARTIFACT_PATH("artifactPath"),

    RESOURCE_URL("resourceUrl"),

    REPOSITORY_ID("repositoryId"),

    STORAGE_ID("storageId"),

    REFRESH_CONTENT("refreshContent");

    private String name;

    private RepositoryFileAttributeType(String name) {
        this.name = name;
    }

    public String getName() {
        return name;
    }
    private static final Map<String, RepositoryFileAttributeType> NAME_TO_VALUE_MAP;

    static {
        Map<String, RepositoryFileAttributeType> map = new HashMap<>();
        for (RepositoryFileAttributeType value : values()) {
            map.put(value.name, value);
        }
        NAME_TO_VALUE_MAP = Collections.unmodifiableMap(map);
    }

    public static RepositoryFileAttributeType of(String s)
    {
        RepositoryFileAttributeType result = NAME_TO_VALUE_MAP.get(s);
        if (result == null) {
            throw new IllegalArgumentException("No enum constant for string: " + s);
        }
        return result;
    }

}
