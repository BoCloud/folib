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
