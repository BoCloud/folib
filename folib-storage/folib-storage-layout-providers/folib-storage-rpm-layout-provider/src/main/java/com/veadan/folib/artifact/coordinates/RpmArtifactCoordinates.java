package com.veadan.folib.artifact.coordinates;

import javax.validation.constraints.NotBlank;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;

import com.veadan.folib.artifact.coordinates.versioning.SemanticVersion;
import com.veadan.folib.db.schema.Vertices;
import com.veadan.folib.domain.LayoutArtifactCoordinatesEntity;
import com.veadan.folib.domain.RpmPackageArch;
import com.veadan.folib.domain.RpmPackageType;
import com.veadan.folib.util.RpmArtifactCoordinatesUtils;
import org.codehaus.commons.nullanalysis.NotNull;
import org.neo4j.ogm.annotation.NodeEntity;

/**
 * This class is an {@link ArtifactCoordinates} implementation for RPM-packages.
 *
 * There are two types of RPM packages. One of them is default binary RPM,
 * that contain prebuild binaries for your platform. Other way is source RPM (SRPM)
 * SRPM package contain source code, patches to it, and SPEC file, which describes
 * how to build the source code into a binary RPM.
 * Be attention - SRPM packages have SRC suffix instead architecture describing.
 *
 *  The canonical named package is represented to below structure:
 * {name}-{version}-{release}.{architecture}.rpm
 *
 * Examples:
 * somepackage-1.0-1.x86_64.rpm - binary distribution with Arch suffix;
 * somepackage-1.0-1.src.rpm    - SRPM package with SRC suffix;
 *
 * @author Ilya Shatalov <ilya@alov.me>
 */
@NodeEntity(Vertices.RPM_ARTIFACT_COORDINATES)
@XmlRootElement(name = "RpmArtifactCoordinates")
@XmlAccessorType(XmlAccessType.NONE)
@ArtifactCoordinatesLayout(name = RpmArtifactCoordinates.LAYOUT_NAME, alias = RpmArtifactCoordinates.LAYOUT_ALIAS)
public class RpmArtifactCoordinates extends LayoutArtifactCoordinatesEntity<RpmArtifactCoordinates, RpmArtifactCoordinates>
{
    public static final String LAYOUT_NAME = "rpm";

    public static final String LAYOUT_ALIAS = "rpm";

    public static final String BASE_NAME = "base_name";

    public static final String VERSION = "version";

    public static final String RELEASE = "release";

    public static final String ARCHITECTURE = "architecture";

    public static final String PACKAGE_TYPE = "package_type";

    public static final String EXTENSION = "extension";

    public static final String DEFAULT_EXTENSION = "rpm";


    public RpmArtifactCoordinates(@NotBlank String baseName,
                                  @NotBlank String version,
                                  @NotBlank String release,
                                  @NotNull RpmPackageType packageType,
                                  @NotNull RpmPackageArch arch)
    {
        this();
        setId(baseName);
        setVersion(version);
        setRelease(release);
        setPackageType(packageType);

        if (packageType == RpmPackageType.BINARY)
        {
            setArchitecture(arch);
        }
        setExtension();
    }

    public RpmArtifactCoordinates(@NotBlank String baseName,
                                  @NotBlank String version,
                                  @NotBlank String release,
                                  @NotNull RpmPackageType packageType)
    {
        this();
        setId(baseName);
        setVersion(version);
        setRelease(release);
        setPackageType(packageType);
        setExtension();
    }

    public RpmArtifactCoordinates()
    {
        resetCoordinates(BASE_NAME, VERSION, RELEASE, ARCHITECTURE, EXTENSION);
    }

    @Override
    public String getId()
    {
        return getCoordinate(BASE_NAME);
    }

    @Override
    public RpmArtifactCoordinates getNativeVersion() {
        return null;
    }

    public void setId(String id)
    {
        setCoordinate(BASE_NAME, id);
    }

    public String getRelease()
    {
        return getCoordinate(RELEASE);
    }

    public void setRelease(String release)
    {
        setCoordinate(RELEASE, release);
    }

    public String getArchitecture()
    {
        return getCoordinate(ARCHITECTURE);
    }

    public void setArchitecture(RpmPackageArch arch)
    {
        setCoordinate(ARCHITECTURE, arch.getName());
    }

    public void setPackageType(RpmPackageType packageType)
    {
        setCoordinate(PACKAGE_TYPE, packageType.getPostfix());
    }

    public void setExtension()
    {
        setCoordinate(EXTENSION, DEFAULT_EXTENSION);
    }


    public String getPackageType()
    {
        return getCoordinate(PACKAGE_TYPE);
    }

    public String getExtension()
    {
        return getCoordinate(EXTENSION);
    }


    //@Override
    //public SemanticVersion getNativeVersion()
    //{
    //    String version = getVersion();
    //
    //    return version == null || version.isEmpty()
    //            ? null
    //            : SemanticVersion.parse(version);
    //}

    @Override
    public String convertToPath(RpmArtifactCoordinates c)
    {
        String path;
        if (RpmPackageType.SOURCE.getPostfix().equals(c.getPackageType()))
        {
            path = String.format("%s-%s-%s.%s.%s",
                                 c.getId(),
                                 c.getVersion(),
                                 c.getRelease(),
                                 c.getPackageType(),
                                 c.getExtension());
        }
        else
        {
            path = String.format("%s-%s-%s.%s.%s",
                                 c.getId(),
                                 c.getVersion(),
                                 c.getRelease(),
                                 c.getArchitecture(),
                                 c.getExtension());
        }

        return path;
    }

    /**
     * @param path The filename of the RPM-package.
     * @return Returns a RpmArtifactCoordinates object with all included  coordinates set
     */
    public static RpmArtifactCoordinates parse(String path)
    {
        return RpmArtifactCoordinatesUtils.parse(path);
    }
    public static String calculatePackageId(String packageScope, String packageName)
    {
        return packageScope == null ? packageName : String.format("%s/%s", packageScope, packageName);
    }

    public static RpmArtifactCoordinates of(String packageId,
                                            String version)
    {
        if (packageId.contains("/"))
        {
            String[] nameSplit = packageId.split("/");

            return new RpmArtifactCoordinates(nameSplit[0], nameSplit[1], version, RpmPackageType.SOURCE);
        }

        return new RpmArtifactCoordinates(null, packageId, version, RpmPackageType.BINARY);
    }

    public String getName() {
        return "test";

    }

    public String getScope() {
        return "dev";
    }
}
