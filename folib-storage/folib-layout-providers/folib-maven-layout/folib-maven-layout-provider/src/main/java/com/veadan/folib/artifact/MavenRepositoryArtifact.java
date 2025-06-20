package com.veadan.folib.artifact;

import com.veadan.folib.providers.io.RepositoryPath;

import java.io.File;
import java.util.Collection;
import java.util.List;
import java.util.Optional;

import org.apache.commons.lang3.StringUtils;
import org.apache.maven.artifact.Artifact;
import org.apache.maven.artifact.handler.ArtifactHandler;
import org.apache.maven.artifact.metadata.ArtifactMetadata;
import org.apache.maven.artifact.repository.ArtifactRepository;
import org.apache.maven.artifact.resolver.filter.ArtifactFilter;
import org.apache.maven.artifact.versioning.ArtifactVersion;
import org.apache.maven.artifact.versioning.VersionRange;
import org.apache.maven.index.artifact.Gav;
import org.apache.maven.index.artifact.VersionUtils;

/**
 * @author veadan
 */
public class MavenRepositoryArtifact
        implements MavenArtifact
{

    private String groupId;

    private String artifactId;

    private String version;

    private String type;

    private String classifier;

    private String baseVersion;

    private RepositoryPath path;

    public MavenRepositoryArtifact(Gav gav,
                                   RepositoryPath path)
    {
        this(gav);
        this.path = path;
    }

    public MavenRepositoryArtifact(Gav gav)
    {
        this(gav.getGroupId(),
             gav.getArtifactId(),
             gav.getVersion(),
             gav.getExtension(),
             gav.getClassifier());
    }

    public MavenRepositoryArtifact(final String groupId,
                                   final String artifactId,
                                   final String version,
                                   final String type)
    {
        this(groupId, artifactId, version, type, null);
    }

    public MavenRepositoryArtifact(final String groupId,
                                   final String artifactId,
                                   final String version,
                                   final String type,
                                   final String classifier)
    {
        this.groupId = groupId;
        this.artifactId = artifactId;
        this.version = version;
        this.type = type;
        this.classifier = classifier;
    }

    @Override
    public File getFile()
    {
        throw new UnsupportedOperationException("Use getPath instead");
    }

    @Override
    public void setFile(File destination)
    {
        throw new UnsupportedOperationException("Use setPath instead");
    }

    @Override
    public String getBaseVersion()
    {
        return baseVersion;
    }

    @Override
    public void setBaseVersion(String baseVersion)
    {
        this.baseVersion = baseVersion;
    }

    @Override
    public String getId()
    {
        throw new UnsupportedOperationException("unsupported");
    }

    @Override
    public String getDependencyConflictId()
    {
        throw new UnsupportedOperationException("unsupported");
    }

    @Override
    public void addMetadata(ArtifactMetadata metadata)
    {
        throw new UnsupportedOperationException("unsupported");
    }

    @Override
    public Collection<ArtifactMetadata> getMetadataList()
    {
        throw new UnsupportedOperationException("unsupported");
    }

    @Override
    public void setRepository(ArtifactRepository remoteRepository)
    {
        throw new UnsupportedOperationException("unsupported");
    }

    @Override
    public ArtifactRepository getRepository()
    {
        throw new UnsupportedOperationException("unsupported");
    }

    @Override
    public void updateVersion(String version,
                              ArtifactRepository localRepository)
    {
        throw new UnsupportedOperationException("unsupported");
    }

    @Override
    public String getDownloadUrl()
    {
        throw new UnsupportedOperationException("unsupported");
    }

    @Override
    public void setDownloadUrl(String downloadUrl)
    {
        throw new UnsupportedOperationException("unsupported");
    }

    @Override
    public ArtifactFilter getDependencyFilter()
    {
        throw new UnsupportedOperationException("unsupported");
    }

    @Override
    public void setDependencyFilter(ArtifactFilter artifactFilter)
    {
        throw new UnsupportedOperationException("unsupported");
    }

    @Override
    public ArtifactHandler getArtifactHandler()
    {
        throw new UnsupportedOperationException("unsupported");
    }

    @Override
    public List<String> getDependencyTrail()
    {
        throw new UnsupportedOperationException("unsupported");
    }

    @Override
    public void setDependencyTrail(List<String> dependencyTrail)
    {
        throw new UnsupportedOperationException("unsupported");
    }

    @Override
    public void setScope(String scope)
    {
        throw new UnsupportedOperationException("unsupported");
    }

    @Override
    public VersionRange getVersionRange()
    {
        throw new UnsupportedOperationException("unsupported");
    }

    @Override
    public void setVersionRange(VersionRange newRange)
    {
        throw new UnsupportedOperationException("unsupported");
    }

    @Override
    public void selectVersion(String version)
    {
        throw new UnsupportedOperationException("unsupported");
    }

    @Override
    public void setGroupId(String groupId)
    {
        this.groupId = groupId;
    }

    @Override
    public void setArtifactId(String artifactId)
    {
        this.artifactId = artifactId;
    }

    @Override
    public boolean isSnapshot()
    {
        return VersionUtils.isSnapshot(version);
    }

    @Override
    public void setResolved(boolean resolved)
    {
        throw new UnsupportedOperationException("unsupported");
    }

    @Override
    public boolean isResolved()
    {
        throw new UnsupportedOperationException("unsupported");
    }

    @Override
    public void setResolvedVersion(String version)
    {
        throw new UnsupportedOperationException("unsupported");
    }

    @Override
    public void setArtifactHandler(ArtifactHandler handler)
    {
        throw new UnsupportedOperationException("unsupported");
    }

    @Override
    public boolean isRelease()
    {
        return !isSnapshot();
    }

    @Override
    public void setRelease(boolean release)
    {
        throw new UnsupportedOperationException("unsupported");
    }

    @Override
    public List<ArtifactVersion> getAvailableVersions()
    {
        throw new UnsupportedOperationException("unsupported");
    }

    @Override
    public void setAvailableVersions(List<ArtifactVersion> versions)
    {
        throw new UnsupportedOperationException("unsupported");
    }

    @Override
    public boolean isOptional()
    {
        throw new UnsupportedOperationException("unsupported");
    }

    @Override
    public void setOptional(boolean optional)
    {
        throw new UnsupportedOperationException("unsupported");
    }

    @Override
    public ArtifactVersion getSelectedVersion()
    {
        throw new UnsupportedOperationException("unsupported");
    }

    @Override
    public boolean isSelectedVersionKnown()
    {
        throw new UnsupportedOperationException("unsupported");
    }

    @Override
    public RepositoryPath getPath()
    {
        return path;
    }

    @Override
    public void setPath(final RepositoryPath path)
    {
        this.path = path;
    }

    @Override
    public String getGroupId()
    {
        return groupId;
    }

    @Override
    public String getArtifactId()
    {
        return artifactId;
    }

    @Override
    public String getVersion()
    {
        return version;
    }

    @Override
    public void setVersion(String version)
    {
        this.version = version;
    }

    @Override
    public String getScope()
    {
        throw new UnsupportedOperationException("unsupported");
    }

    @Override
    public String getType()
    {
        if (type != null && type.trim().length() > 0)
        {
            return type;
        }
        if (path == null)
        {
            return null;
        }

        String fileName = path.getFileName().toString();
        return Optional.of(fileName.lastIndexOf('.'))
                       .map(i -> (i == -1) ? "" : fileName.substring(i + 1))
                       .get();
    }

    @Override
    public String getClassifier()
    {
        return classifier;
    }

    @Override
    public boolean hasClassifier()
    {
        return classifier != null;
    }

    @Override
    public int compareTo(Artifact o)
    {
        int compare = StringUtils.compare(artifactId, o.getArtifactId());
        if (compare != 0)
        {
            return compare;
        }
        compare = StringUtils.compare(groupId, o.getGroupId());
        if (compare != 0)
        {
            return compare;
        }
        compare = StringUtils.compare(version, o.getVersion());
        if (compare != 0)
        {
            return compare;
        }
        compare = StringUtils.compare(type, o.getType());
        if (compare != 0)
        {
            return compare;
        }
        return StringUtils.compare(classifier, o.getClassifier());
    }
}
