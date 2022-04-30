package com.veadan.folib.providers.io;

import com.veadan.folib.artifact.coordinates.ArtifactCoordinates;

import java.net.URL;
import java.nio.file.attribute.BasicFileAttributes;
import java.nio.file.attribute.FileTime;
import java.util.HashMap;
import java.util.Map;

public class RepositoryFileAttributes
        implements BasicFileAttributes
{

    private BasicFileAttributes basicAttributes;

    private Map<RepositoryFileAttributeType, Object> attributes = new HashMap<>();

    public RepositoryFileAttributes(BasicFileAttributes basicAttributes)
    {
        super();
        this.basicAttributes = basicAttributes;
    }

    public RepositoryFileAttributes(BasicFileAttributes basicAttributes,
                                    Map<RepositoryFileAttributeType, Object> attributes)
    {
        super();
        this.basicAttributes = basicAttributes;
        this.attributes = attributes;
    }

    public FileTime lastModifiedTime()
    {
        return basicAttributes.lastModifiedTime();
    }

    public FileTime lastAccessTime()
    {
        return basicAttributes.lastAccessTime();
    }

    public FileTime creationTime()
    {
        return basicAttributes.creationTime();
    }

    public boolean isRegularFile()
    {
        return basicAttributes.isRegularFile();
    }

    public boolean isDirectory()
    {
        return basicAttributes.isDirectory();
    }

    public boolean isSymbolicLink()
    {
        return basicAttributes.isSymbolicLink();
    }

    public boolean isOther()
    {
        return basicAttributes.isOther();
    }

    public long size()
    {
        return basicAttributes.size();
    }

    public Object fileKey()
    {
        return basicAttributes.fileKey();
    }

    public ArtifactCoordinates getCoordinates()
    {
        return (ArtifactCoordinates) attributes.get(RepositoryFileAttributeType.COORDINATES);
    }

    protected void setCoordinates(ArtifactCoordinates coordinates)
    {
        attributes.put(RepositoryFileAttributeType.COORDINATES, coordinates);
    }

    public boolean isMetadata()
    {
        return Boolean.TRUE.equals(attributes.get(RepositoryFileAttributeType.METADATA));
    }

    protected void setMetadata(boolean isMetadata)
    {
        attributes.put(RepositoryFileAttributeType.METADATA, isMetadata);
    }

    public boolean isChecksum()
    {
        return Boolean.TRUE.equals(attributes.get(RepositoryFileAttributeType.CHECKSUM));
    }

    protected void setChecksum(boolean isChecksum)
    {
        attributes.put(RepositoryFileAttributeType.CHECKSUM, isChecksum);
    }

    public boolean isTrash()
    {
        return Boolean.TRUE.equals(attributes.get(RepositoryFileAttributeType.TRASH));
    }

    protected void setTrash(boolean isTrash)
    {
        attributes.put(RepositoryFileAttributeType.TRASH, isTrash);
    }

    public boolean isTemp()
    {
        return Boolean.TRUE.equals(attributes.get(RepositoryFileAttributeType.TEMP));
    }

    protected void setTemp(boolean isTemp)
    {
        attributes.put(RepositoryFileAttributeType.TEMP, isTemp);
    }

    public boolean isArtifact()
    {
        return Boolean.TRUE.equals(attributes.get(RepositoryFileAttributeType.ARTIFACT));
    }

    protected void setArtifact(boolean isArtifact)
    {
        attributes.put(RepositoryFileAttributeType.ARTIFACT, isArtifact);
    }

    public boolean hasExpired()
    {
        return Boolean.TRUE.equals(attributes.get(RepositoryFileAttributeType.EXPIRED));
    }

    public boolean getResourceUrl()
    {
        return Boolean.TRUE.equals(attributes.get(RepositoryFileAttributeType.RESOURCE_URL));
    }

    protected void setResourceUrl(URL url)
    {
        attributes.put(RepositoryFileAttributeType.RESOURCE_URL, url);
    }

    public boolean getArtifactPath()
    {
        return Boolean.TRUE.equals(attributes.get(RepositoryFileAttributeType.ARTIFACT_PATH));
    }

    protected void setArtifactPath(String path)
    {
        attributes.put(RepositoryFileAttributeType.ARTIFACT_PATH, path);
    }

    public String getStorageId()
    {
        return (String) attributes.get(RepositoryFileAttributeType.STORAGE_ID);
    }

    protected void setStorageId(String id)
    {
        attributes.put(RepositoryFileAttributeType.STORAGE_ID, id);
    }

    public String getRepositoryId()
    {
        return (String) attributes.get(RepositoryFileAttributeType.REPOSITORY_ID);
    }

    public void setRepositoryId(String id)
    {
        attributes.put(RepositoryFileAttributeType.REPOSITORY_ID, id);
    }

}
