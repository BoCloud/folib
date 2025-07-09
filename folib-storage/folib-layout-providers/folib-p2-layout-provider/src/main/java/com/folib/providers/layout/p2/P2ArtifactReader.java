package com.folib.providers.layout.p2;

import com.folib.artifact.coordinates.P2Coordinates;

import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Paths;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Finds a {@link P2Coordinates} from the provided repository base directory and artifact path.
 * The artifact path consists of {id}/{version}/{classifier}. E.g. bundle-name/1.0.0/osgi.bundle
 */
public class P2ArtifactReader
{

    private static final Logger logger = LoggerFactory.getLogger(P2ArtifactReader.class);

    /**
     * Finds a {@link P2Coordinates} from the provided repository base directory and artifact path.
     *
     * @param repositoryBaseDir The folder containing the repository
     * @param bundle            The artifact path in the format of {id}/{version}/{classifier}. E.g. bundle-name/1.0.0/osgi.bundle
     * @return P2ArtifactCoordinates if found otherwise null
     */
    public static P2Coordinates getArtifact(String repositoryBaseDir,
                                            String bundle)
            throws IOException
    {
        // TODO we are dropping GenericParser and one day we will need to support P2
        //GenericParser<P2Repository> repositoryParser = new GenericParser<>(P2Repository.class);
        P2Repository p2Repository = null;//repositoryParser.parse(createPath(repositoryBaseDir).toUri().toURL());
        final P2Coordinates artifactToFind = P2Coordinates.create(bundle);
        for (P2Artifact p2Artifact : p2Repository.getArtifacts().getArtifacts())
        {
            P2Coordinates foundArtifact = new P2Coordinates(p2Artifact.getId(),
                                                                            p2Artifact.getVersion(),
                                                                            p2Artifact.getClassifier());
            if (foundArtifact.equals(artifactToFind))
            {
                addProperties(foundArtifact, p2Artifact, repositoryBaseDir);
                String bundleFilename = P2ArtifactRuleProcessor.getFilename(p2Repository.getMappings(),
                                                                            foundArtifact);
                foundArtifact.setFilename(bundleFilename);
                return foundArtifact;
            }
        }

        return null;
    }

    private static void addProperties(P2Coordinates foundArtifact,
                                      P2Artifact p2Artifact,
                                      String baseDir)
    {
        foundArtifact.addProperty("repoUrl", baseDir);
        foundArtifact.addProperty("id", p2Artifact.getId());
        foundArtifact.addProperty("version", p2Artifact.getVersion());
        foundArtifact.addProperty("classifier", p2Artifact.getClassifier());
        P2Properties properties = p2Artifact.getProperties();
        if (properties != null)
        {
            properties.getPropertites().forEach(
                    property -> foundArtifact.addProperty(property.getName(), property.getValue()));
        }
    }

    private static Path createPath(String repositoryBaseDir)
    {
        final String artifactsFilename = "artifacts.xml";
        if (repositoryBaseDir == null || repositoryBaseDir.isEmpty())
        {
            return Paths.get(artifactsFilename);
        }

        return Paths.get(repositoryBaseDir).resolve(artifactsFilename);
    }
}
