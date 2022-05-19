package com.veadan.folib.artifact.coordinates;

import com.veadan.folib.db.schema.Vertices;
import com.veadan.folib.domain.LayoutArtifactCoordinatesEntity;
import org.apache.commons.lang3.StringUtils;
import org.neo4j.ogm.annotation.NodeEntity;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;
import java.net.URI;
import java.util.Arrays;
import java.util.Map;
import java.util.Objects;
import java.util.stream.Collectors;

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

    //public static final String REPOSITORY = "repository";

    public static final String TAG = "tag";

    public static final String LAYERS = "layers";

    public static  final String ARTIFACT_PATH= "path";


    public DockerArtifactCoordinates()
    {
        resetCoordinates(LAYERS,
                TAG);
    }


    //
    // TODO: We will have to think about something like this:
    //


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

    public DockerArtifactCoordinates(String repository,
                                     String reference,
                                     String layers,
                                     String artifactPath)
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
        setLayers(layers);
        setArtifactPath(artifactPath);

        // TODO:
        // setLayers(layers);
    }

    public static DockerArtifactCoordinates parse(String path)
    {
        // TODO:
        if(Objects.isNull(path)){
            return null;
        }
        String [] strings = path.split("/");
        String repository  =  strings[0];
        String tag = strings[strings.length-2];
        String artifactPath = ARTIFACT_PATH;

        String layers = LAYERS;
        if(strings[strings.length-1].indexOf("sha256:")>-1){
            layers = strings[strings.length-1];
            String finalLayers = layers;
            artifactPath = Arrays.stream(strings).filter(data->!Objects.equals(finalLayers,data) || !Objects.equals(repository,data))
                    .collect(Collectors.joining("/"));

        }


        return  new DockerArtifactCoordinates(repository,tag,layers,artifactPath);
    }

    @Override
    public String getId()
    {
        return getLayers();
    }


    public void setId(String id)
    {
        setLayers(id);
    }

    @Override
    public String getVersion()
    {
        return getTAG();
    }

    @Override
    public String getPath() {
        return super.getPath();
    }

    @Override
    public void setVersion(String version)
    {
        setTAG(version);
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
        return  artifactCoordinates.getArtifactPath();
       // return new  DockerArtifactGenerator(artifactCoordinates.getPath()).getImageManifestPath().toString();
      //  return String.format("%s/%s/%s/%s", artifactCoordinates.g, c.getName(), c.getVersion(), c.getArtifactFileName());
    }

    @Override
    public URI convertToResource(DockerArtifactCoordinates artifactCoordinates) {
        return super.convertToResource(artifactCoordinates);
    }


    @Override
    public void setUuid(String uuid) {
        super.setUuid(uuid);
    }


    @ArtifactLayoutCoordinate
    public String getTAG()
    {
        return getCoordinate(TAG);
    }

    public  void setTAG(String tag) {
        setCoordinate(TAG, tag);
    }

    @ArtifactLayoutCoordinate
    public  String getLayers() {
        return getCoordinate(LAYERS);
    }

    public  void setLayers(String layers) {
        setCoordinate(LAYERS,layers);
    }


    @ArtifactLayoutCoordinate
    public  String getArtifactPath() {
        return getCoordinate(ARTIFACT_PATH);
    }

    public void  setArtifactPath(String artifactPath) {
        setCoordinate(ARTIFACT_PATH,artifactPath);
    }


}
