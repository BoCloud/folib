package com.folib.artifact.coordinates;


import jakarta.xml.bind.annotation.XmlAccessType;
import jakarta.xml.bind.annotation.XmlAccessorType;
import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlRootElement;
import org.apache.commons.lang3.StringUtils;
import org.apache.maven.artifact.Artifact;
import org.apache.maven.artifact.versioning.ComparableVersion;
import com.folib.artifact.MavenArtifact;
import com.folib.artifact.MavenArtifactUtils;
import com.folib.artifact.MavenRepositoryArtifact;
import com.veadan.folib.db.schema.Vertices;
import com.folib.domain.LayoutArtifactCoordinatesEntity;
import org.neo4j.ogm.annotation.NodeEntity;

/**
 * @author Veadan
 */
@NodeEntity(Vertices.MAVEN_ARTIFACT_COORDINATES)
@XmlRootElement(name = "maven-artifact-coordinates")
@XmlAccessorType(XmlAccessType.NONE)
@ArtifactCoordinatesLayout(name = MavenArtifactCoordinates.LAYOUT_NAME, alias = MavenArtifactCoordinates.LAYOUT_ALIAS)
public class MavenArtifactCoordinates
        extends LayoutArtifactCoordinatesEntity<MavenArtifactCoordinates, ComparableVersion>
{

    public static final String LAYOUT_NAME = "Maven 2";

    public static final String LAYOUT_ALIAS = "maven";

    private static final String GROUPID = "groupId";

    private static final String ARTIFACTID = "artifactId";

    private static final String VERSION = "version";

    private static final String CLASSIFIER = "classifier";

    private static final String EXTENSION = "extension";

    public MavenArtifactCoordinates()
    {
        resetCoordinates(GROUPID, ARTIFACTID, VERSION, CLASSIFIER, EXTENSION);
    }

    public MavenArtifactCoordinates(String... coordinateValues)
    {
        this();

        int i = 0;
        for (String coordinateValue : coordinateValues)
        {
            // Please, forgive the following construct...
            // (In my defense, I felt equally stupid and bad for doing it this way):
            switch (i)
            {
                case 0:
                    setGroupId(coordinateValue);
                    break;
                case 1:
                    setArtifactId(coordinateValue);
                    break;
                case 2:
                    setVersion(coordinateValue);
                    break;
                case 3:
                    setClassifier(coordinateValue);
                    break;
                case 4:
                    setExtension(coordinateValue);
                    break;
                default:
                    break;
            }

            i++;
        }

        if (getExtension() == null)
        {
            setExtension("jar");
        }

    }

    public MavenArtifactCoordinates(Artifact artifact)
    {
        setGroupId(artifact.getGroupId());
        setArtifactId(artifact.getArtifactId());
        setVersion(artifact.getVersion());
        setClassifier(artifact.getClassifier());

        String type = artifact.getType();
        if (StringUtils.isNotBlank(type))
        {
            setExtension(type);
        }
        else
        {
            setExtension("jar");
        }

    }
    
    @Override
    public String convertToPath(MavenArtifactCoordinates c)
    {
        return MavenArtifactUtils.convertArtifactToPath(c.toArtifact());
    }

    public MavenArtifact toArtifact()
    {
        return new MavenRepositoryArtifact(getGroupId(), getArtifactId(), getVersion(), getExtension(), getClassifier());
    }

    @ArtifactLayoutCoordinate
    @XmlAttribute(name = "groupId")
    public String getGroupId()
    {
        return getCoordinate(GROUPID);
    }

    public void setGroupId(String groupId)
    {
        setCoordinate(GROUPID, groupId);
    }

    @ArtifactLayoutCoordinate
    @XmlAttribute(name = "artifactId")
    public String getArtifactId()
    {
        return getCoordinate(ARTIFACTID);
    }

    public void setArtifactId(String artifactId)
    {
        setCoordinate(ARTIFACTID, artifactId);
    }

    @Override
    public String getId()
    {
        return String.format("%s:%s", getGroupId(), getArtifactId());
    }

    public void setId(String id)
    {
        setArtifactId(id);
    }

    @Override
    @XmlAttribute(name = "version")
    public String getVersion()
    {
        return super.getVersion();
    }

    @ArtifactLayoutCoordinate
    @XmlAttribute(name = "classifier")
    public String getClassifier()
    {
        return getCoordinate(CLASSIFIER);
    }

    public void setClassifier(String classifier)
    {
        setCoordinate(CLASSIFIER, classifier);
    }

    @ArtifactLayoutCoordinate
    @XmlAttribute(name = "extension")
    public String getExtension()
    {
        return getCoordinate(EXTENSION);
    }

    public void setExtension(String extension)
    {
        setCoordinate(EXTENSION, extension);
    }

    @Override
    public ComparableVersion getNativeVersion()
    {
        String versionLocal = getVersion();
        if (versionLocal == null)
        {
            return null;
        }
        return new ComparableVersion(versionLocal);
    }

    @Override
    public String toString()
    {
        return "MavenArtifactCoordinates{" + "groupId='" + getGroupId() + '\'' + ", artifactId='" + getArtifactId() + '\'' +
               ", version='" + getVersion() + '\'' + ", classifier='" + getClassifier() + '\'' + ", extension='" + getExtension() +
               '\'' + ", as path: " + convertToPath(this) + '}';
    }

}
