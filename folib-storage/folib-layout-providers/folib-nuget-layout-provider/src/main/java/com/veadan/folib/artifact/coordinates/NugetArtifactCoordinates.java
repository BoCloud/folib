package com.veadan.folib.artifact.coordinates;

import java.net.URI;
import java.util.regex.Matcher;
import java.util.regex.Pattern;


import com.veadan.folib.artifact.coordinates.versioning.SemanticVersion;
import com.veadan.folib.db.schema.Vertices;
import com.veadan.folib.domain.LayoutArtifactCoordinatesEntity;
import jakarta.xml.bind.annotation.XmlAccessType;
import jakarta.xml.bind.annotation.XmlAccessorType;
import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlRootElement;
import org.neo4j.ogm.annotation.NodeEntity;
import org.springframework.util.Assert;

/**
 * @author @author veadan
 *
 */
@NodeEntity(Vertices.NUGET_ARTIFACT_COORDINATES)
@XmlRootElement(name = "nugetArtifactCoordinates")
@XmlAccessorType(XmlAccessType.NONE)
@ArtifactCoordinatesLayout(name = NugetArtifactCoordinates.LAYOUT_NAME, alias = NugetArtifactCoordinates.LAYOUT_ALIAS)
public class NugetArtifactCoordinates
        extends LayoutArtifactCoordinatesEntity<NugetArtifactCoordinates, SemanticVersion>
{

    public static final String LAYOUT_NAME = "NuGet";

    public static final String LAYOUT_ALIAS = "nuget";
    
    public static final String ID = "id";

    public static final String VERSION = "version";

    public static final String EXTENSION = "extension";

    private static final String DEFAULT_EXTENSION = "nupkg";

    private static final String NUGET_PACKAGE_REGEXP_PATTERN = "([a-zA-Z0-9_.-]+)/([a-zA-Z0-9_.-]+)/([a-zA-Z0-9_.-]+).(nupkg|nuspec|nupkg\\.sha512)";

    private static final Pattern NUGET_PACKAGE_REGEXP = Pattern.compile(NUGET_PACKAGE_REGEXP_PATTERN);
    
    public NugetArtifactCoordinates()
    {
        resetCoordinates(ID, VERSION, EXTENSION);
    }

    public NugetArtifactCoordinates(String id,
                                    String version)
    {
        this(id, version, DEFAULT_EXTENSION);
    }

    public NugetArtifactCoordinates(String id,
                                    String version,
                                    String type)
    {
        this();
        setId(id);
        setVersion(version);
        setType(type);
    }

    @Override
    @XmlAttribute(name="id")
    @ArtifactLayoutCoordinate
    public String getId()
    {
        return getCoordinate(ID);
    }

    public void setId(String id)
    {
        setCoordinate(ID, id);
    }

    @Override
    @XmlAttribute(name="version")
    public String getVersion()
    {
        return super.getVersion();
    }

    @ArtifactLayoutCoordinate
    @XmlAttribute(name="type")
    public String getType()
    {
        return getCoordinate(EXTENSION);
    }

    public void setType(String type)
    {
        setCoordinate(EXTENSION, type);
    }
    
    @Override
    public String convertToPath(NugetArtifactCoordinates c)
    {
        String idLocal = c.getId();
        String versionLocal = c.getVersion();
        String typeLocal = c.getType();

        if ("nuspec".equals(typeLocal))
        {
            return String.format("%s/%s/%s.%s", idLocal, versionLocal, idLocal, typeLocal);
        }

        return String.format("%s/%s/%s.%s.%s", idLocal, versionLocal, idLocal, versionLocal, typeLocal);
    }

    @Override
    public URI convertToResource(NugetArtifactCoordinates c)
    {
        return URI.create("package/" + c.getId() + "/" + c.getVersion());
    }
    
    @Override
    public SemanticVersion getNativeVersion()
    {
        String versionLocal = getVersion();
        if (versionLocal == null)
        {
            return null;
        }
        try
        {
            return SemanticVersion.parse(versionLocal);
        }
        catch (IllegalArgumentException e)
        {
            return null;
        }
    }
    
    public static NugetArtifactCoordinates parse(String path)
    {
        Matcher matcher = NUGET_PACKAGE_REGEXP.matcher(path);

        Assert.isTrue(matcher.matches(), String.format("Illegal artifact path [%s].", path));
        
        String packageId = matcher.group(1);
        String version = matcher.group(2);
        String packageArtifactName = matcher.group(3);
        String packageArtifactType = matcher.group(4);

        Assert.isTrue(String.format("%s.%s", packageId, version).startsWith(packageArtifactName),
                      String.format("Illegal artifact path [%s].", path));

        return new NugetArtifactCoordinates(packageId, version, packageArtifactType);
    }
    
}
