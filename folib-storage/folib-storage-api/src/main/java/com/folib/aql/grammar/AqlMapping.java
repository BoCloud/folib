package com.folib.aql.grammar;

public enum AqlMapping
{

    STORAGE("storageId"),
    REPOSITORY("repositoryId"),
    LAYOUT("artifactCoordinates.@class"),
    VERSION("artifactCoordinates.version"),
    TAG("tagSet.name"),
    FROM("lastUpdated"),
    TO("lastUpdated"),
    AGE("lastUpdated");

    private String property;

    private AqlMapping(String property)
    {
        this.property = property;
    }

    public String property()
    {
        return property;
    }

}
