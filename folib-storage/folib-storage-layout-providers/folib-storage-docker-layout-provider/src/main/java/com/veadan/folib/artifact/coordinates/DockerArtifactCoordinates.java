package com.veadan.folib.artifact.coordinates;

import com.veadan.folib.artifact.coordinates.versioning.SemanticVersion;

import javax.persistence.Entity;
import javax.validation.constraints.NotBlank;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;
import java.net.URI;
import java.util.List;
import java.util.Map;

import com.veadan.folib.db.schema.Vertices;
import com.veadan.folib.domain.LayoutArtifactCoordinatesEntity;
import org.apache.commons.lang3.StringUtils;
import org.codehaus.commons.nullanalysis.NotNull;
import org.neo4j.ogm.annotation.NodeEntity;

/**
 * @author carlspring
 */
@NodeEntity(Vertices.DOCKER_ARTIFACT_COORDINATES)
@XmlRootElement(name = "DockerArtifactCoordinates")
@XmlAccessorType(XmlAccessType.NONE)
@ArtifactCoordinatesLayout(name = DockerArtifactCoordinates.LAYOUT_NAME, alias = DockerArtifactCoordinates.LAYOUT_ALIAS)
public class DockerArtifactCoordinates
        extends LayoutArtifactCoordinatesEntity<DockerArtifactCoordinates, String>
{

    public static final String LAYOUT_NAME = "Docker";

    public static final String LAYOUT_ALIAS = "Docker";

    public static final String REPOSITORY = "repository";

    public static final String TAG = "tag";


    public DockerArtifactCoordinates()
    {
        resetCoordinates(REPOSITORY,
                TAG);
    }
    //
    // TODO: We will have to think about something like this:
    //
    // public static final String LAYERS = "layers";

    public DockerArtifactCoordinates(String repository,
                                     String reference)
    {
        // if any of the required arguments are empty, throw an error
        if (StringUtils.isBlank(repository))
        {
            throw new IllegalArgumentException("The repository field is mandatory.");
        }

        if (StringUtils.isBlank(reference))
        {
            throw new IllegalArgumentException("The reference field is mandatory.");
        }

        setId(repository);
        setVersion(reference);

        // TODO:
        // setLayers(layers);
    }

    public static DockerArtifactCoordinates parse(String path)
    {
        // TODO:
        return null;
    }

    @Override
    public String getId()
    {
        return getCoordinate(REPOSITORY);
    }


    public void setId(String id)
    {
        setCoordinate(REPOSITORY, id);
    }

    @Override
    public String getVersion()
    {
        return getCoordinate(TAG);
    }

    @Override
    public String getPath() {
        return super.getPath();
    }

    @Override
    public void setVersion(String version)
    {
        setCoordinate(TAG, version);
    }


    /**
     * @return Returns the reconstructed path from the stored coordinate values
     */

    public String toPath()
    {
        // TODO:
        return null;
    }

    /**
     * @return Returns the native version of the package
     */
    @Override
    public String getNativeVersion()
    {
        return getVersion();
    }

    /**
     * @return Returns a map data structure of the coordinates without the TAG coordinate
     */

    public Map<String, String> dropVersion()
    {
        Map<String, String> result = getCoordinates();
        result.remove(TAG);

        return result;
    }

    @Override
    public GenericArtifactCoordinates getHierarchyChild() {
        return super.getHierarchyChild();
    }

    @Override
    public void setHierarchyChild(GenericArtifactCoordinates node) {
        super.setHierarchyChild(node);
    }

    @Override
    public String convertToPath(DockerArtifactCoordinates artifactCoordinates) {
        return new DockerArtifactGenerator(artifactCoordinates.getPath()).getImageManifestPath().toString();
    }

    @Override
    public URI convertToResource(DockerArtifactCoordinates artifactCoordinates) {
        return super.convertToResource(artifactCoordinates);
    }


}
