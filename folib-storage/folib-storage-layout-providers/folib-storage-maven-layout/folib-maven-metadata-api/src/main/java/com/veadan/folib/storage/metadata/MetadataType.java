package com.veadan.folib.storage.metadata;

/**
 * @author mtodorov
 */
public enum MetadataType
{

    /**
     * Used for artifact root-level metadata.
     *
     * (For example, "org.foo:bar:2.0" --> "org/foo/bar/maven-metadata.xml).
     */
    ARTIFACT_ROOT_LEVEL("artifact"),

    /**
     * Used for artifact root-level metadata.
     *
     * (For example, "org.foo:bar:2.0-SNAPSHOT" --> "org/foo/bar/2.0/maven-metadata.xml).
     */
    SNAPSHOT_VERSION_LEVEL("snapshot"),

    /**
     * Used for artifact root-level metadata.
     *
     * (For example, "org.foo:some-maven-plugin:1.2.3" --> "org/foo/maven-metadata.xml).
     */
    PLUGIN_GROUP_LEVEL("plugin");

    private String type;


    MetadataType(String type)
    {
        this.type = type;
    }

    public static MetadataType from(String metadataType)
            throws IllegalArgumentException, UnsupportedOperationException
    {
        if (metadataType == null)
        {
            throw new IllegalArgumentException("Invalid metadata type!");
        }

        if (MetadataType.ARTIFACT_ROOT_LEVEL.getType().equals(metadataType))
        {
            return ARTIFACT_ROOT_LEVEL;
        }
        else if (MetadataType.SNAPSHOT_VERSION_LEVEL.getType().equals(metadataType))
        {
            return SNAPSHOT_VERSION_LEVEL;
        }
        else if (MetadataType.PLUGIN_GROUP_LEVEL.getType().equals(metadataType))
        {
            return PLUGIN_GROUP_LEVEL;
        }

        throw new UnsupportedOperationException("Unsupported metadata type!");
    }

    public String getType()
    {
        return type;
    }

}
