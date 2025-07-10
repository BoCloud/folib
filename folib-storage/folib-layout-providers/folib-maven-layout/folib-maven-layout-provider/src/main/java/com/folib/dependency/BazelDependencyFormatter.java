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
package com.folib.dependency;

import javax.annotation.PostConstruct;
import javax.inject.Inject;

import com.folib.artifact.coordinates.ArtifactCoordinates;
import com.folib.dependency.snippet.CompatibleDependencyFormatRegistry;
import com.folib.dependency.snippet.DependencySynonymFormatter;
import com.folib.providers.layout.AbstractLayoutProvider;
import com.folib.artifact.coordinates.MavenCoordinates;
import com.folib.providers.layout.Maven2LayoutProvider;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

/**
 * 
 * @author Declan-Y
 *
 */
@Component
public class BazelDependencyFormatter
        implements DependencySynonymFormatter
{
    private static final Logger logger = LoggerFactory.getLogger(AbstractLayoutProvider.class);
    
    public static final String ALIAS = "Bazel";
    
    @Inject
    private CompatibleDependencyFormatRegistry compatibleDependencyFormatRegistry;
    
    
    @PostConstruct
    @Override
    public void register()
    {
        compatibleDependencyFormatRegistry.addProviderImplementation(getLayout(), getFormatAlias(), this);
        
        logger.info("Initialized the Bazel dependency formatter.");
    }
    @Override
    public String getLayout()
    {
        return Maven2LayoutProvider.ALIAS;
    }
    
    
    @Override
    public String getFormatAlias()
    {
        return ALIAS;
    }
    
    @Override
    public String getDependencySnippet(ArtifactCoordinates artifactCoordinates)
    {
        MavenCoordinates coordinates = (MavenCoordinates) artifactCoordinates;
        
        return "maven_jar(" +"\n"+"    name ="+" "
        +"\""+coordinates.getArtifactId()+"\""+","+
        (coordinates.getArtifactId() != null && coordinates.getGroupId() != null && coordinates.getVersion() != null ? 
        "\n    artifact = " + "\""+
        coordinates.getGroupId()+":"+coordinates.getArtifactId()+":"+coordinates.getVersion()+"\""+",\n)\n": 
        "\n)\n");
    }

}
