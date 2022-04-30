package com.veadan.folib.storage.metadata.maven.versions;

import java.nio.file.attribute.FileTime;
import java.util.ArrayList;
import java.util.List;

import org.apache.maven.artifact.repository.metadata.SnapshotVersion;

/**
 * @author Steve Todorov <s.todorov@itnews-bg.com>
 */
public class MetadataVersion implements Comparable<MetadataVersion>
{

    private String version;

    private FileTime createdDate;

    private List<SnapshotVersion> snapshots = new ArrayList<>();

    public String getVersion()
    {
        return version;
    }

    public void setVersion(String version)
    {
        this.version = version;
    }

    public FileTime getCreatedDate()
    {
        return createdDate;
    }

    public void setCreatedDate(FileTime createdDate)
    {
        this.createdDate = createdDate;
    }

    public List<SnapshotVersion> getSnapshots()
    {
        return snapshots;
    }

    public void setSnapshots(List<SnapshotVersion> snapshots)
    {
        this.snapshots = snapshots;
    }

    @Override
    public int compareTo(MetadataVersion v1)
    {
        int diff = v1.getVersion().compareTo(this.getVersion());
        if (diff < 1 && this.createdDate.toMillis() > v1.getCreatedDate().toMillis())
        {
            diff = 1;
        }

        return diff;
    }

    @Override
    public boolean equals(Object o)
    {
        if (this == o)
        {
            return true;
        }
        if (o == null || getClass() != o.getClass())
        {
            return false;
        }

        MetadataVersion that = (MetadataVersion) o;

        if (version != null ? !version.equals(that.version) : that.version != null)
        {
            return false;
        }
        if (createdDate != null ? !createdDate.equals(that.createdDate) : that.createdDate != null)
        {
            return false;
        }

        return snapshots != null ? snapshots.equals(that.snapshots) : that.snapshots == null;
    }

    @Override
    public int hashCode()
    {
        int result = version != null ? version.hashCode() : 0;
        result = 31 * result + (createdDate != null ? createdDate.hashCode() : 0);
        result = 31 * result + (snapshots != null ? snapshots.hashCode() : 0);

        return result;
    }

}
