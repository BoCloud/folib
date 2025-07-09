package com.folib.domain;

import java.util.Arrays;
import java.util.LinkedHashMap;
import java.util.Map;

import com.folib.artifact.coordinates.GenericCoordinates;
import com.folib.data.domain.DomainEntity;
import com.folib.db.schema.Edges;
import com.folib.db.schema.Vertices;
import org.neo4j.ogm.annotation.NodeEntity;
import org.neo4j.ogm.annotation.Properties;
import org.neo4j.ogm.annotation.Relationship;

@NodeEntity(Vertices.GENERIC_COORDINATES)
public class GenericCoordinatesEntity extends DomainEntity implements GenericCoordinates
{
    private String version;
    @Properties
    private final Map<String, String> coordinates = new LinkedHashMap<>();
    @Relationship(type = Edges.EXTENDS, direction = Relationship.INCOMING)
    private GenericCoordinates layoutArtifactCoordinates;

    @Override
    public void setHierarchyChild(GenericCoordinates child) {
        this.layoutArtifactCoordinates = child;
    }
    
    @Override
    public GenericCoordinates getHierarchyChild()
    {
        return layoutArtifactCoordinates;
    }

    protected void resetCoordinates(String... coordinates)
    {
        this.coordinates.clear();
        Arrays.stream(coordinates).forEach(this::defineCoordinate);
    }

    protected void defineCoordinate(String coordinate)
    {
        coordinates.put(coordinate, null);
    }

    protected String getCoordinate(String coordinate)
    {
        return coordinates.get(coordinate);
    }

    public String setCoordinate(String coordinate,
                                String value)
    {
        return coordinates.put(coordinate, value);
    }

    @Override
    public String getVersion()
    {
        return version;
    }

    public void setVersion(String version)
    {
        this.version = version;
    }

    public Map<String, String> getCoordinates()
    {
        return coordinates;
    }

    @Override
    public boolean equals(Object obj)
    {
        if (!(obj instanceof GenericCoordinatesEntity))
        {
            return false;
        }

        GenericCoordinatesEntity c = (GenericCoordinatesEntity) obj;
        return c.coordinates.equals(coordinates);
    }

    @Override
    public int hashCode()
    {
        return coordinates.hashCode();
    }

}
