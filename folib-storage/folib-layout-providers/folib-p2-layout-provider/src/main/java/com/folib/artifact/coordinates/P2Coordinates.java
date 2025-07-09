package com.folib.artifact.coordinates;

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

import com.folib.domain.LayoutCoordinatesEntity;

/**
 * Represents {@link ArtifactCoordinates} for P2 repository
 * <p>
 * Proper path for this coordinates is in the format of: {id}/{version}/{classifier}
 * Example: folib.p2/1.0.0/osgi.bundle
 */
@CoordinatesLayout("p2")
public class P2Coordinates
        extends LayoutCoordinatesEntity<P2Coordinates, P2Coordinates>
{

    public static final String ID = "id";

    public static final String VERSION = "version";

    public static final String CLASSIFIER = "classifier";

    public static final String FILENAME = "filename";

    private static final String SEPARATOR = "/";

    private Map<String, String> properties = new HashMap<>();

    public P2Coordinates(String id,
                         String version,
                         String classifier)
    {
        if (id == null || version == null || classifier == null)
        {
            throw new IllegalArgumentException("Id, version and classifier must be specified");
        }
        setId(id);
        setVersion(version);
        setCoordinate(CLASSIFIER, classifier);
    }

    @Override
    public String getId()
    {
        return getCoordinate(ID);
    }

    public void setId(String id)
    {
        setCoordinate(ID, id);
    }

    public String getClassifier()
    {
        return getCoordinate(CLASSIFIER);
    }

    @Override
    public String convertToPath(P2Coordinates c)
    {
        return getId() + SEPARATOR + c.getVersion() + SEPARATOR + c.getClassifier();
    }

    public void setFilename(String filename)
    {
        setCoordinate(FILENAME, filename);
    }

    public String getFilename()
    {
        return getCoordinate(FILENAME);
    }

    public static P2Coordinates create(String path)
    {
        if (path != null && path.length() > 0 && path.contains(SEPARATOR))
        {
            String[] splitedSeparator = path.split("/");
            if (splitedSeparator.length == 3)
            {
                return new P2Coordinates(splitedSeparator[0], splitedSeparator[1], splitedSeparator[2]);
            }
        }

        throw new IllegalArgumentException("Path is not properly formatted");
    }

    public void addProperty(String key,
                            String value)
    {
        properties.put(key, value);
    }

    public Map<String, String> getProperties()
    {
        return Collections.unmodifiableMap(properties);
    }

    @Override
    public boolean equals(Object o)
    {
        if (this == o)
        {
            return true;
        }
        if (o == null || getClass() != o.getClass())
        {
            return false;
        }

        P2Coordinates that = (P2Coordinates) o;

        if (!getId().equals(that.getId()))
        {
            return false;
        }
        if (!getVersion().equals(that.getVersion()))
        {
            return false;
        }

        return getClassifier().equals(that.getClassifier());
    }

    @Override
    public int hashCode()
    {
        int result = getId().hashCode();
        result = 31 * result + getVersion().hashCode();
        result = 31 * result + getClassifier().hashCode();

        return result;
    }

    @Override
    public P2Coordinates getNativeVersion()
    {
        return null;
    }
    
}
