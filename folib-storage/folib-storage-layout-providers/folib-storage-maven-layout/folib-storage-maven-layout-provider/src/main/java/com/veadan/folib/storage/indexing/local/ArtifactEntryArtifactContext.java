package com.veadan.folib.storage.indexing.local;

import java.io.File;

import com.veadan.folib.artifact.coordinates.MavenArtifactCoordinates;
import com.veadan.folib.domain.Artifact;
import org.apache.commons.io.FilenameUtils;
import org.apache.maven.index.ArtifactContext;
import org.apache.maven.index.ArtifactInfo;
import org.apache.maven.index.artifact.Gav;
import org.apache.maven.model.Model;
import com.veadan.folib.domain.MavenArtifactEntryUtils;
import org.codehaus.plexus.util.StringUtils;

/**
 * @author veadan
 */
public class ArtifactEntryArtifactContext
        extends ArtifactContext
{

    private final Artifact artifactEntry;
    private final ArtifactEntryArtifactContextHelper artifactEntryArtifactContextHelper;

    public ArtifactEntryArtifactContext(final Artifact artifactEntry,
                                        final ArtifactEntryArtifactContextHelper artifactEntryArtifactContextHelper)
            throws IllegalArgumentException
    {
        super(null, null, null, asArtifactInfo(artifactEntry), asGav(artifactEntry));
        this.artifactEntry = artifactEntry;
        this.artifactEntryArtifactContextHelper = artifactEntryArtifactContextHelper;
    }

    private static ArtifactInfo asArtifactInfo(Artifact artifactEntry)
    {
        final MavenArtifactCoordinates coordinates = (MavenArtifactCoordinates) artifactEntry.getArtifactCoordinates();
        ArtifactInfo artifactInfo = new ArtifactInfo(artifactEntry.getRepositoryId(),
                                                     coordinates.getGroupId(),
                                                     coordinates.getArtifactId(),
                                                     coordinates.getVersion(),
                                                     coordinates.getClassifier(),
                                                     coordinates.getExtension());

        produce(coordinates, artifactInfo);

        return artifactInfo;
    }

    /**
     * @see org.apache.maven.index.DefaultArtifactContextProducer#getArtifactContext(org.apache.maven.index.context.IndexingContext, java.io.File)
     */
    private static void produce(MavenArtifactCoordinates coordinates,
                                ArtifactInfo artifactInfo)
    {
        if (!StringUtils.isEmpty(coordinates.getClassifier()))
        {
            artifactInfo.setPackaging(coordinates.getExtension());
        }

        artifactInfo.setFileName(FilenameUtils.getName(coordinates.buildPath()));
        artifactInfo.setFileExtension(coordinates.getExtension());
    }

    private static Gav asGav(Artifact artifactEntry)
    {
        return MavenArtifactEntryUtils.toGav(artifactEntry);
    }

    public Artifact getArtifactEntry()
    {
        return artifactEntry;
    }

    @Override
    public File getArtifact()
    {
        throw new UnsupportedOperationException("This ArtifactContext base on ArtifactEntry");
    }

    @Override
    public File getMetadata()
    {
        throw new UnsupportedOperationException("This ArtifactContext base on ArtifactEntry");
    }

    @Override
    public File getPom()
    {
        throw new UnsupportedOperationException("This ArtifactContext base on ArtifactEntry");
    }

    @Override
    public Model getPomModel()
    {
        throw new UnsupportedOperationException("This ArtifactContext base on ArtifactEntry");
    }

    public boolean pomExists()
    {
        return artifactEntryArtifactContextHelper.pomExists();
    }

    public boolean sourcesExists()
    {
        return artifactEntryArtifactContextHelper.sourcesExists();
    }

    public boolean javadocExists()
    {
        return artifactEntryArtifactContextHelper.javadocExists();
    }
}
