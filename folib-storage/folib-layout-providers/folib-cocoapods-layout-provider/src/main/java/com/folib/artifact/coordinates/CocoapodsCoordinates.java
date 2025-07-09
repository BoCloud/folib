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
package com.folib.artifact.coordinates;

import com.folib.db.schema.Vertices;
import com.folib.domain.LayoutCoordinatesEntity;
import jakarta.xml.bind.annotation.XmlAccessType;
import jakarta.xml.bind.annotation.XmlAccessorType;
import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlRootElement;
import org.neo4j.ogm.annotation.NodeEntity;



/**
 * @author veadan
 * @date 2023/7/28 17:44
 */
@NodeEntity(Vertices.COCOAPODS_COORDINATES)
@XmlRootElement(name = "CocoapodsCoordinates")
@XmlAccessorType(XmlAccessType.NONE)
@CoordinatesLayout(name = CocoapodsCoordinates.LAYOUT_NAME, alias = CocoapodsCoordinates.LAYOUT_ALIAS)
public class CocoapodsCoordinates extends
        LayoutCoordinatesEntity<CocoapodsCoordinates, String> {

    public static final String LAYOUT_NAME = "cocoapods";

    public static final String LAYOUT_ALIAS = LAYOUT_NAME;
    
    private static final String NAME = "name";
    private static final String BASE_NAME = "base_name";
    private static final String PATH = "path";


    public CocoapodsCoordinates() {
        resetCoordinates(NAME, PATH);
    }
    
    public CocoapodsCoordinates(String name) {
        this();
        
        this.setName(name);
    }
    
    public CocoapodsCoordinates(String name, String path) {
        this();
        
        this.setName(name);
        this.setPath(path);
    }

    @Override
    public String getId() {
        return this.getName();
    }

    @Override
    public String getNativeVersion() {
        return super.getVersion();
    }

    @Override
    public String convertToPath(CocoapodsCoordinates artifactCoordinates) {
        return artifactCoordinates.getId();
    }


    @ArtifactLayoutCoordinate
    @XmlAttribute(name = NAME)
    public String getName()
    {
        return getCoordinate(NAME);
    }

    public void setName(String name)
    {
        setCoordinate(NAME, name);
    }

    @ArtifactLayoutCoordinate
    @XmlAttribute(name = BASE_NAME)
    public String getBaseName()
    {
        return getCoordinate(BASE_NAME);
    }

    public void setBaseName(String baseName) {
        setCoordinate(BASE_NAME, baseName);
    }
    
    @ArtifactLayoutCoordinate
    @XmlAttribute(name = PATH)
    public String getPath()
    {
        return getCoordinate(PATH);
    }

    public void setPath(String path) {
        setCoordinate(PATH, path);
        
    }
}
