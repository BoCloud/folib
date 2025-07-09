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


package com.folib.storage.metadata.nuget;

import jakarta.xml.bind.annotation.XmlElement;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

/**
 * Package dependencies
 * 
 * @author Dmitry Veadan
 */
public class Dependencies implements Serializable
{
//
//    /**
//     * Direct dependencies
//     */
//    @XmlElement(name = "dependency", namespace = Nuspec.NUSPEC_XML_NAMESPACE_2011)
//    public List<Dependency> dependencies;
//
//    /**
//     * Dependency groups
//     */
//    @XmlElement(name = "group", namespace = Nuspec.NUSPEC_XML_NAMESPACE_2011)
//    private List<DependenciesGroup> groups;

    /**
     * Default constructor
     */
    public Dependencies()
    {
        // JAX-B
    }

    /**
     * Constructor for setting dependency values
     *
     * @param dependencies
     *            direct dependencies
     * @param groups
     *            group dependencies
     */
//    public Dependencies(List<Dependency> dependencies,
//                        List<DependenciesGroup> groups)
//    {
//        this.dependencies = dependencies;
//        this.groups = groups;
//    }

    /**
     * @return package dependencies, including those in groups
     */
//    public List<Dependency> getDependencies()
//    {
//        if (dependencies == null)
//        {
//            dependencies = new ArrayList<>();
//        }
//
//        List<Dependency> result = new ArrayList<>();
//        result.addAll(dependencies);
//
//        if (groups != null)
//        {
//            for (DependenciesGroup group : groups)
//            {
//                result.addAll(group.getDependencies());
//            }
//        }
//
//        return result;
//    }

    /**
     * @return dependency groups including root
     */
//    public List<DependenciesGroup> getGroups()
//    {
//        if (groups == null)
//        {
//            groups = new ArrayList<>();
//        }
//
//        if (dependencies != null && !dependencies.isEmpty())
//        {
//            DependenciesGroup rootGroup = new DependenciesGroup();
//            rootGroup.setDependencies(dependencies);
//
//            ArrayList<DependenciesGroup> result = new ArrayList<>(groups.size() + 1);
//            result.addAll(groups);
//            result.add(rootGroup);
//
//            return result;
//        }
//        else
//        {
//            return groups;
//        }
//    }
}
