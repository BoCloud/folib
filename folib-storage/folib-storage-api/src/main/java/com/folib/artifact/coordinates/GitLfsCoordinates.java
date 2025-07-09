package com.folib.artifact.coordinates;


import com.folib.db.schema.Vertices;
import com.folib.domain.LayoutCoordinatesEntity;
import jakarta.xml.bind.annotation.XmlAccessType;
import jakarta.xml.bind.annotation.XmlAccessorType;
import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlRootElement;
import org.neo4j.ogm.annotation.NodeEntity;


@NodeEntity(Vertices.GITLFS_COORDINATES)
@XmlRootElement(name = "GitLfsCoordinates")
@XmlAccessorType(XmlAccessType.NONE)
@CoordinatesLayout(name = GitLfsCoordinates.LAYOUT_NAME, alias = GitLfsCoordinates.LAYOUT_ALIAS)
public class GitLfsCoordinates extends LayoutCoordinatesEntity<GitLfsCoordinates, GitLfsCoordinates>
{

    public static final String LAYOUT_NAME = "GitLfs";
    public static final String LAYOUT_ALIAS = LAYOUT_NAME;
    private static final String PATH = "path";

    public GitLfsCoordinates()
    {
        resetCoordinates(PATH);
    }

    public GitLfsCoordinates(String path)
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
    public GitLfsCoordinates getNativeVersion()
    {
        return this;
    }

    @Override
    public String convertToPath(GitLfsCoordinates artifactCoordinates)
    {

        return artifactCoordinates.getId();
    }

}
