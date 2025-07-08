package com.folib.storage.indexing.local;

import java.util.Set;

import com.folib.artifact.coordinates.MavenArtifactCoordinates;
import com.folib.domain.Artifact;
import com.folib.domain.ArtifactArchiveListing;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.maven.index.ArtifactContext;
import org.apache.maven.index.ArtifactInfo;
import org.apache.maven.index.creator.JarFileContentsIndexCreator;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * @author veadan
 */
public class ArtifactEntryJarFileContentsIndexCreator
        extends JarFileContentsIndexCreator
{

    public static final ArtifactEntryJarFileContentsIndexCreator INSTANCE = new ArtifactEntryJarFileContentsIndexCreator();

    private ArtifactEntryJarFileContentsIndexCreator()
    {
        super();
    }

    private static final Logger logger = LoggerFactory.getLogger(ArtifactEntryJarFileContentsIndexCreator.class);

    @Override
    public void populateArtifactInfo(ArtifactContext artifactContext)
    {
        ArtifactEntryArtifactContext artifactEntryArtifactContext = (ArtifactEntryArtifactContext) artifactContext;
        Artifact artifactEntry = artifactEntryArtifactContext.getArtifactEntry();
        ArtifactInfo artifactInfo = artifactEntryArtifactContext.getArtifactInfo();

        final MavenArtifactCoordinates coordinates = (MavenArtifactCoordinates) artifactEntry.getArtifactCoordinates();
        final String extension = coordinates.getExtension();

        if ("jar" .equals(extension) ||
            "war" .equals(extension) ||
            "zip" .equals(extension))
        {
            updateArtifactInfo(artifactInfo, artifactEntry);
        }
    }

    /**
     * @see JarFileContentsIndexCreator#updateArtifactInfo(org.apache.maven.index.ArtifactInfo, java.io.File)
     */
    private void updateArtifactInfo(final ArtifactInfo artifactInfo,
                                    final Artifact artifactEntry)
    {
        final MavenArtifactCoordinates coordinates = (MavenArtifactCoordinates) artifactEntry.getArtifactCoordinates();

        String strippedPrefix = null;
        if ("war" .equals(coordinates.getExtension()))
        {
            strippedPrefix = "WEB-INF/classes/";
        }

        updateArtifactInfo(artifactInfo, artifactEntry, strippedPrefix);
    }

    /**
     * @see org.apache.maven.index.creator.JarFileContentsIndexCreator#updateArtifactInfo(org.apache.maven.index.ArtifactInfo, java.io.File, java.lang.String)
     */
    private void updateArtifactInfo(final ArtifactInfo artifactInfo,
                                    final Artifact artifactEntry,
                                    final String strippedPrefix)
    {
        ArtifactArchiveListing artifactArchiveListing = artifactEntry.getArtifactArchiveListing();
        if (artifactArchiveListing == null || CollectionUtils.isEmpty(artifactArchiveListing.getFilenames()))
        {
            return;
        }

        Set<String> filenames = artifactArchiveListing.getFilenames();

        final StringBuilder sb = new StringBuilder();

        for (final String name : filenames)
        {
            if (name.endsWith(".class"))
            {
                // original maven indexer skips inner classes too
                final int i = name.indexOf("$");

                if (i == -1)
                {
                    if (name.charAt(0) != '/')
                    {
                        sb.append('/');
                    }

                    if (StringUtils.isBlank(strippedPrefix))
                    {
                        // class name without ".class"
                        sb.append(name, 0, name.length() - 6).append('\n');
                    }
                    else if (name.startsWith(strippedPrefix)
                             && (name.length() > (strippedPrefix.length() + 6)))
                    {
                        // class name without ".class" and stripped prefix
                        sb.append(name, strippedPrefix.length(), name.length() - 6).append('\n');
                    }
                }
            }
        }

        final String fieldValue = sb.toString().trim();

        logger.info("Updating ArtifactInfo using artifactEntry [{}] by classNames [{}]",
                     artifactEntry,
                     fieldValue);

        if (fieldValue.length() != 0)
        {
            artifactInfo.setClassNames(fieldValue);
        }
        else
        {
            artifactInfo.setClassNames(null);
        }
    }
}
