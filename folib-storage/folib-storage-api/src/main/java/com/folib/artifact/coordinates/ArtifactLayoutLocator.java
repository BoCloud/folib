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

import java.util.HashMap;
import java.util.Map;

import org.reflections.Reflections;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * With this class you can get all available Layouts from classpath.
 * 
 * 
 * @author veadan
 * 
 * @see CoordinatesLayout
 * @see ArtifactLayoutCoordinate
 * @see ArtifactCoordinates
 * 
 */
public class ArtifactLayoutLocator
{

    private static final Logger logger = LoggerFactory.getLogger(ArtifactLayoutLocator.class);

    private volatile static Map<String, ArtifactLayoutDescription> layoutByAliasEntityMap;
    private volatile static Map<String, ArtifactLayoutDescription> layoutByNameEntityMap;

    public static Map<String, ArtifactLayoutDescription> getLayoutEntityMap()
    {
        if (layoutByAliasEntityMap != null)
        {
            return layoutByAliasEntityMap;
        }
        
        locateAvaliableLayouts();
        
        return layoutByAliasEntityMap;
    }

    public static Map<String, ArtifactLayoutDescription> getLayoutByNameEntityMap()
    {
        if (layoutByNameEntityMap != null)
        {
            return layoutByNameEntityMap;
        }
        
        locateAvaliableLayouts();
        
        return layoutByNameEntityMap;
    }
    
    private static synchronized void locateAvaliableLayouts()
    {
        Map<String, ArtifactLayoutDescription> layoutByAliasEntityMapLocal = new HashMap<>();
        Map<String, ArtifactLayoutDescription> layoutByNameEntityMapLocal = new HashMap<>();

        Reflections reflections = new Reflections("com.folib.artifact.coordinates");
        for (Class<? extends ArtifactCoordinates> artifactCoordinatesClass : reflections.getSubTypesOf(ArtifactCoordinates.class))
        {
            CoordinatesLayout artifactLayout = artifactCoordinatesClass.getAnnotation(CoordinatesLayout.class);
            if (artifactLayout == null)
            {
                logger.warn("[{}] should be provided for [{}] class",
                            CoordinatesLayout.class.getSimpleName(),
                            artifactCoordinatesClass.getSimpleName());
                continue;
            }

            ArtifactLayoutDescription layoutDesc = ArtifactLayoutDescription.parse(artifactCoordinatesClass);
            layoutByAliasEntityMapLocal.put(layoutDesc.getLayoutAlias(), layoutDesc);
            layoutByNameEntityMapLocal.put(layoutDesc.getLayoutName(), layoutDesc);
        }

        layoutByAliasEntityMap = layoutByAliasEntityMapLocal;
        layoutByNameEntityMap = layoutByNameEntityMapLocal;
    }

}
