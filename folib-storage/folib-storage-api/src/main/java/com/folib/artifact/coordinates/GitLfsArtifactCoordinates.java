package com.folib.artifact.coordinates;


import com.veadan.folib.db.schema.Vertices;
import com.folib.domain.LayoutArtifactCoordinatesEntity;
import jakarta.xml.bind.annotation.XmlAccessType;
import jakarta.xml.bind.annotation.XmlAccessorType;
import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlRootElement;
import org.neo4j.ogm.annotation.NodeEntity;


@NodeEntity(Vertices.GITLFS_ARTIFACT_COORDINATES)
@XmlRootElement(name = "GitLfsArtifactCoordinates")
@XmlAccessorType(XmlAccessType.NONE)
@ArtifactCoordinatesLayout(name = GitLfsArtifactCoordinates.LAYOUT_NAME, alias = GitLfsArtifactCoordinates.LAYOUT_ALIAS)
public class GitLfsArtifactCoordinates extends LayoutArtifactCoordinatesEntity<GitLfsArtifactCoordinates, GitLfsArtifactCoordinates>
{

    public static final String LAYOUT_NAME = "GitLfs";
    public static final String LAYOUT_ALIAS = LAYOUT_NAME;
    private static final String PATH = "path";

    public GitLfsArtifactCoordinates()
    {
        resetCoordinates(PATH);
    }

    public GitLfsArtifactCoordinates(String path)
    {
        setCoordinate(PATH, path);
    }

    @Override
    public String getId()
    {
        return getCoordinate(PATH);
    }

    public void setId(String id)
    {
        setCoordinate(PATH, id);
    }

    @Override
    @ArtifactLayoutCoordinate
    @XmlAttribute(name = "path")
    public String getPath()
    {
        return getId();
    }

    /**
     * WARNING: Unsurprisingly, this is null.
     * @return  null
     */
    @Override
    public String getVersion()
    {
        return null;
    }

    @Override
    public void setVersion(String version)
    {
    }

    @Override
    public GitLfsArtifactCoordinates getNativeVersion()
    {
        return this;
    }

    @Override
    public String convertToPath(GitLfsArtifactCoordinates artifactCoordinates)
    {

        return artifactCoordinates.getId();
    }

}
