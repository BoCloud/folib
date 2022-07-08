package com.veadan.folib.domain;

import java.net.URI;
import java.util.Collections;
import java.util.Map;

import com.veadan.folib.artifact.coordinates.ArtifactCoordinates;
import com.veadan.folib.artifact.coordinates.ArtifactCoordinatesComparator;
import com.veadan.folib.artifact.coordinates.ArtifactCoordinatesResourceConverter;
import com.veadan.folib.artifact.coordinates.GenericArtifactCoordinates;
import com.veadan.folib.data.domain.DomainEntity;
import com.veadan.folib.db.schema.Edges;
import org.neo4j.ogm.annotation.Relationship;

/**
 * @author xuxinping
 */
public abstract class LayoutArtifactCoordinatesEntity<C extends LayoutArtifactCoordinatesEntity<C, V>, V extends Comparable<V>>
        extends DomainEntity
        implements ArtifactCoordinates<C, V>, ArtifactCoordinatesResourceConverter<C, V>
{

    @Relationship(type = Edges.EXTENDS, direction = Relationship.OUTGOING)
    private GenericArtifactCoordinatesEntity genericArtifactCoordinates;
    private final ArtifactCoordinatesComparator<C, V> comparator = new ArtifactCoordinatesComparator<>();

    public LayoutArtifactCoordinatesEntity()
    {
        genericArtifactCoordinates = new GenericArtifactCoordinatesEntity();
        genericArtifactCoordinates.setHierarchyChild(this);
    }

    @Override
    public GenericArtifactCoordinates getHierarchyParent()
    {
        return genericArtifactCoordinates;
    }

    @Override
    public void setHierarchyParent(GenericArtifactCoordinates node)
    {
        this.genericArtifactCoordinates = (GenericArtifactCoordinatesEntity) node;
    }

    public void setUuid(String uuid)
    {
        super.setUuid(uuid);
        genericArtifactCoordinates.setUuid(uuid);
    }

    public String getVersion()
    {
        return genericArtifactCoordinates.getVersion();
    }

    public void setVersion(String version)
    {
        genericArtifactCoordinates.setVersion(version);
    }

    public Map<String, String> getCoordinates()
    {
        return Collections.unmodifiableMap(genericArtifactCoordinates.getCoordinates());
    }

    protected void resetCoordinates(String... coordinates)
    {
        genericArtifactCoordinates.resetCoordinates(coordinates);
    }

    protected void defineCoordinate(String coordinate)
    {
        genericArtifactCoordinates.defineCoordinate(coordinate);
    }

    protected String setCoordinate(String coordinate,
                                   String value)
    {
        return genericArtifactCoordinates.setCoordinate(coordinate, value);
    }

    protected String getCoordinate(String coordinate)
    {
        return genericArtifactCoordinates.getCoordinate(coordinate);
    }

    @Override
    public final String buildPath()
    {
        setUuid(convertToPath((C) this));
        return getUuid();
    }

    @Override
    public final URI buildResource()
    {
        return convertToResource((C) this);
    }

    @Override
    public int compareTo(C that)
    {
        return comparator.compare((C) this, that);
    }

    public boolean equals(Object obj)
    {
        return genericArtifactCoordinates.equals(obj);
    }

    public int hashCode()
    {
        return genericArtifactCoordinates.hashCode();
    }

}
