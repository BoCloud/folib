/*
 * Copyright 2019 Carlspring Consulting & Development Ltd.
 * Copyright 2014 Dmitry Sviridov
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.veadan.folib.storage.metadata.nuget;

import javax.xml.bind.annotation.XmlElement;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

/**
 * Package dependencies
 * 
 * @author Dmitry Sviridov
 */
public class Dependencies implements Serializable
{

    /**
     * Direct dependencies
     */
    @XmlElement(name = "dependency", namespace = Nuspec.NUSPEC_XML_NAMESPACE_2011)
    public List<Dependency> dependencies;

    /**
     * Dependency groups
     */
    @XmlElement(name = "group", namespace = Nuspec.NUSPEC_XML_NAMESPACE_2011)
    private List<DependenciesGroup> groups;

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
    public Dependencies(List<Dependency> dependencies,
                        List<DependenciesGroup> groups)
    {
        this.dependencies = dependencies;
        this.groups = groups;
    }

    /**
     * @return package dependencies, including those in groups
     */
    public List<Dependency> getDependencies()
    {
        if (dependencies == null)
        {
            dependencies = new ArrayList<>();
        }

        List<Dependency> result = new ArrayList<>();
        result.addAll(dependencies);

        if (groups != null)
        {
            for (DependenciesGroup group : groups)
            {
                result.addAll(group.getDependencies());
            }
        }

        return result;
    }

    /**
     * @return dependency groups including root
     */
    public List<DependenciesGroup> getGroups()
    {
        if (groups == null)
        {
            groups = new ArrayList<>();
        }

        if (dependencies != null && !dependencies.isEmpty())
        {
            DependenciesGroup rootGroup = new DependenciesGroup();
            rootGroup.setDependencies(dependencies);

            ArrayList<DependenciesGroup> result = new ArrayList<>(groups.size() + 1);
            result.addAll(groups);
            result.add(rootGroup);

            return result;
        }
        else
        {
            return groups;
        }
    }
}
