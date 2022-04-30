package com.veadan.folib.storage.metadata;

import com.veadan.folib.storage.metadata.maven.versions.MetadataVersion;

import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;

import org.apache.maven.artifact.repository.metadata.Plugin;
import org.apache.maven.artifact.repository.metadata.SnapshotVersion;
import org.apache.maven.artifact.repository.metadata.Versioning;

/**
 * @author mtodorov
 */
public class VersionCollectionRequest
{

    private Path artifactBasePath;

    private Versioning versioning = new Versioning();

    private List<SnapshotVersion> snapshotVersions = new ArrayList<>();

    private List<MetadataVersion> metadataVersions = new ArrayList<>();

    private List<Plugin> plugins = new ArrayList<>();


    public VersionCollectionRequest()
    {
    }

    public Path getArtifactBasePath()
    {
        return artifactBasePath;
    }

    public void setArtifactBasePath(Path artifactBasePath)
    {
        this.artifactBasePath = artifactBasePath;
    }

    public Versioning getVersioning()
    {
        return versioning;
    }

    public void setVersioning(Versioning versioning)
    {
        this.versioning = versioning;
    }

    public List<SnapshotVersion> getSnapshotVersions()
    {
        return snapshotVersions;
    }

    public void setSnapshotVersions(List<SnapshotVersion> snapshotVersions)
    {
        this.snapshotVersions = snapshotVersions;
    }

    public List<Plugin> getPlugins()
    {
        return plugins;
    }

    public void setPlugins(List<Plugin> plugins)
    {
        this.plugins = plugins;
    }

    public boolean addPlugin(Plugin plugin)
    {
        return plugins.add(plugin);
    }

    public List<MetadataVersion> getMetadataVersions()
    {
        return metadataVersions;
    }

    public void setMetadataVersions(List<MetadataVersion> metadataVersions)
    {
        this.metadataVersions = metadataVersions;
    }

}
