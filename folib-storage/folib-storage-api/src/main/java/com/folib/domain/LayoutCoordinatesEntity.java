/*
 * Folib - [新一代AI制品仓库]
 * Copyright (C) 2025 bocloud.com.cn <folib@beyondcent.com>
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * 本程序是自由软件：您可依据GNU通用公共许可证（GPL-3.0+）条款重新发布和修改，
 * 但禁止任何形式的商业售卖行为（包括但不限于：直接销售、捆绑销售、云服务商用）。
 *
 * This program is distributed WITHOUT ANY WARRANTY.
 * Commercial sale of this software is expressly prohibited.
 *
 * For license details, see: https://www.gnu.org/licenses/gpl-3.0.html
 * 商业授权咨询请联系：folib@beyondcent.com
 */
package com.folib.domain;

import java.net.URI;
import java.util.Collections;
import java.util.Map;

import com.folib.artifact.coordinates.ArtifactCoordinates;
import com.folib.artifact.coordinates.ArtifactCoordinatesComparator;
import com.folib.artifact.coordinates.ArtifactCoordinatesResourceConverter;
import com.folib.artifact.coordinates.GenericCoordinates;
import com.folib.data.domain.DomainEntity;
import com.folib.db.schema.Edges;
import org.neo4j.ogm.annotation.Relationship;

/**
 * @author veadan
 */
public abstract class LayoutCoordinatesEntity<C extends LayoutCoordinatesEntity<C, V>, V extends Comparable<V>>
        extends DomainEntity
        implements ArtifactCoordinates<C, V>, ArtifactCoordinatesResourceConverter<C, V>
{

    @Relationship(type = Edges.EXTENDS, direction = Relationship.OUTGOING)
    private GenericCoordinatesEntity genericArtifactCoordinates;
    private final ArtifactCoordinatesComparator<C, V> comparator = new ArtifactCoordinatesComparator<>();

    public LayoutCoordinatesEntity()
    {
        genericArtifactCoordinates = new GenericCoordinatesEntity();
        genericArtifactCoordinates.setHierarchyChild(this);
    }

    @Override
    public GenericCoordinates getHierarchyParent()
    {
        return genericArtifactCoordinates;
    }

    @Override
    public void setHierarchyParent(GenericCoordinates node)
    {
        this.genericArtifactCoordinates = (GenericCoordinatesEntity) node;
    }

    @Override
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
