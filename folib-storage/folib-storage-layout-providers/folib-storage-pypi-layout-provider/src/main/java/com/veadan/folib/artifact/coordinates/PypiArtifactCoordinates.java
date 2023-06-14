package com.veadan.folib.artifact.coordinates;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;

import com.veadan.folib.artifact.coordinates.versioning.SemanticVersion;
import org.apache.commons.lang3.StringUtils;
import com.veadan.folib.db.schema.Vertices;
import com.veadan.folib.domain.LayoutArtifactCoordinatesEntity;
import com.veadan.folib.util.PypiArtifactCoordinatesUtils;
import org.neo4j.ogm.annotation.NodeEntity;

/**
 * This class is an {@link ArtifactCoordinates} implementation for pypi artifacts
 *
 * Proper path for this coordinates is in the format of: 
 * {distribution}-{version}(-{build tag})?-{python tag}-{abi tag}-{platform tag}.whl.
 * for wheel packages and {distribution}-{version}.tar.gz for source packages
 * Examples: distribution-1.0.1-1-py27-none-any.whl, distribution-1.0.1.tar.gz
 * 
 * @author alecg956
 */
@NodeEntity(Vertices.PYPI_ARTIFACT_COORDINATES)
@XmlRootElement(name = "PypiArtifactCoordinates")
@XmlAccessorType(XmlAccessType.NONE)
@ArtifactCoordinatesLayout(name = PypiArtifactCoordinates.LAYOUT_NAME, alias = PypiArtifactCoordinates.LAYOUT_ALIAS)
public class PypiArtifactCoordinates
    extends LayoutArtifactCoordinatesEntity<PypiArtifactCoordinates, SemanticVersion>
{

    public static final String LAYOUT_NAME = "PyPi";

    public static final String LAYOUT_ALIAS = "pypi";

    public static final String DISTRIBUTION = "distribution";

    public static final String VERSION = "version";

    public static final String BUILD = "build";

    public static final String LANGUAGE_IMPLEMENTATION_VERSION = "languageImplementationVersion";

    public static final String ABI = "abi";

    public static final String PLATFORM = "platform";

    public static final String PACKAGING = "packaging";

    public static final String SOURCE_EXTENSION = "tar.gz";

    public static final String WHEEL_EXTENSION = "whl";

    public static final String PATH = "path";

    public PypiArtifactCoordinates()
    {
        resetCoordinates(DISTRIBUTION,
                         VERSION,
                         BUILD,
                         LANGUAGE_IMPLEMENTATION_VERSION,
                         ABI,
                         PLATFORM,
                         PACKAGING);
    }

    /**
     * This method takes in all artifact coordinates of a PyPi package filename, with build being
     * the empty string if it is not included in the filename
     *
     * @param distribution Uniquely identifying artifact coordinate (required)
     * @param version Packages current version (required)
     * @param build Build_tag parameter (optional)
     * @param languageImplementationVersion Language and Implementation version argument (optional)
     * @param abi ABI tag parameter (optional)
     * @param platform Platform tag parameter (optional)
     * @param packaging Packaging of artifact (required)
     */
    public PypiArtifactCoordinates(String distribution,
                                   String version,
                                   String build,
                                   String languageImplementationVersion,
                                   String abi,
                                   String platform,
                                   String packaging)
    {
        this();

        if (StringUtils.isBlank(packaging))
        {
            throw new IllegalArgumentException("The packaging field is mandatory.");
        }

        if (!packaging.equals(SOURCE_EXTENSION) && !packaging.equals(WHEEL_EXTENSION))
        {
            throw new IllegalArgumentException("The artifact has incorrect packaging");
        }

        if (packaging.equals(SOURCE_EXTENSION))
        {
            if (StringUtils.isBlank(distribution) || StringUtils.isBlank(version))
            {
                throw new IllegalArgumentException(
                        "The distribution and version fields are mandatory for source package.");
            }
        }

        if (packaging.equals(WHEEL_EXTENSION))
        {
            if (StringUtils.isBlank(distribution) || StringUtils.isBlank(version) || StringUtils.isBlank(platform)
                || StringUtils.isBlank(languageImplementationVersion) || StringUtils.isBlank(abi))
            {
                throw new IllegalArgumentException("The distribution, version, languageImplementationVersion, abi, and " +
                                                   "platform fields are mandatory for wheel package.");
            }

            if (!StringUtils.isBlank(build) && !Character.isDigit(build.charAt(0)))
            {
                throw new IllegalArgumentException("Illegal build tag!");
            }
        }

        setId(distribution);
        setVersion(version);
        setBuild(build);
        setLanguageImplementationVersion(languageImplementationVersion);
        setAbi(abi);
        setPlatform(platform);
        setPackaging(packaging);
    }

    /**
     * This method takes in all artifact coordinates of a PyPi Wheel filename, with build being
     * the empty string if it is not included in the filename
     *
     * @param distribution Uniquely identifying artifact coordinate (required)
     * @param version Packages current version (required)
     * @param packaging Packaging of artifact (required)
     */
    public PypiArtifactCoordinates(String distribution, String version, String packaging)
    {
        this(distribution, version, null, null, null, null, packaging);
    }

    /**
     * @param path The filename of the PyPi package
     * @return Returns a PyPiArtifactCoordinates object with all included coordinates set
     */
    public static PypiArtifactCoordinates parse(String path)
    {
        return PypiArtifactCoordinatesUtils.parse(path);
    }

    /**
     * @return Returns distribution coordinate value (serves as the unique ID)
     */
    @Override
    public String getId()
    {
        return getCoordinate(DISTRIBUTION);
    }

    /**
     * @param id DISTRIBUTION coordinate will take this value
     */
    public void setId(String id)
    {
        setCoordinate(DISTRIBUTION, id);
    }


    /**
     * @return Returns the BUILD coordinate value
     */
    @ArtifactLayoutCoordinate
    public String getBuild()
    {
        return getCoordinate(BUILD);
    }

    /**
     * @param build BUILD coordinate will take this value
     */
    public void setBuild(String build)
    {
        setCoordinate(BUILD, build);
    }

    /**
     * @param packaging PACKAGING of artifact
     */
    public void setPackaging(String packaging)
    {
        setCoordinate(PACKAGING, packaging);
    }

    /**
     * @return Returns PACKAGING of artifact
     */
    public String getPackaging()
    {
        return getCoordinate(PACKAGING);
    }

    /**
     * @return Returns the LANGUAGE_IMPLEMENTATION_VERSION coordinate value
     */
    @ArtifactLayoutCoordinate
    public String getLanguageImplementationVersion()
    {
        return getCoordinate(LANGUAGE_IMPLEMENTATION_VERSION);
    }

    /**
     * @param lang LANGUAGE_IMPLEMENTATION_VERSION takes this value
     */
    public void setLanguageImplementationVersion(String lang)
    {
        setCoordinate(LANGUAGE_IMPLEMENTATION_VERSION, lang);
    }

    /**
     * @return Returns the ABI coordinate value
     */
    @ArtifactLayoutCoordinate
    public String getAbi()
    {
        return getCoordinate(ABI);
    }

    /**
     * @param abi ABI coordinate takes this value
     */
    public void setAbi(String abi)
    {
        setCoordinate(ABI, abi);
    }

    /**
     * @return Returns the PLATFORM coordinate value
     */
    @ArtifactLayoutCoordinate
    public String getPlatform()
    {
        return getCoordinate(PLATFORM);
    }

    /**
     * @param platform PLATFORM coordinate takes this value
     */
    public void setPlatform(String platform)
    {
        setCoordinate(PLATFORM, platform);
    }

    @ArtifactLayoutCoordinate
    public String getPath() {
        return getCoordinate(PATH);
    }

    /**
     * @param path PLATFORM coordinate takes this value
     */
    public void setPath(String path) {
        setCoordinate(PATH, path);
    }

    /**
     * @return Returns the reconstructed path from the stored coordinate values
     */
    @Override
    public String convertToPath(PypiArtifactCoordinates c)
    {
        String fileName = SOURCE_EXTENSION.equals(c.getPackaging()) ? c.buildSourcePackageFileName()
                                                                  : c.buildWheelPackageFileName();

        return String.format("%s/%s/%s",
                             c.getId(),
                             c.getVersion(),
                             fileName);
    }

    private String buildSourcePackageFileName()
    {
        return String.format("%s-%s.%s",
                             getId(),
                             getVersion(),
                             getPackaging());
    }

    public String buildWheelPackageFileName()
    {
        String path;

        if (StringUtils.isBlank(getBuild()))
        {
            path = String.format("%s-%s-%s-%s-%s.%s",
                                 getId(),
                                 getVersion(),
                                 getLanguageImplementationVersion(),
                                 getAbi(),
                                 getPlatform(), getPackaging());
        }
        else
        {
            path = String.format("%s-%s-%s-%s-%s-%s.%s",
                                 getId(),
                                 getVersion(),
                                 getBuild(),
                                 getLanguageImplementationVersion(),
                                 getAbi(),
                                 getPlatform(),
                                 getPackaging());
        }

        return path;
    }

    public boolean isSourcePackage()
    {
        return SOURCE_EXTENSION.equals(getPackaging());
    }

    /**
     * @return Returns the native version of the package
     */
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

}
