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

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.adapters.XmlAdapter;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

/**
 * @author sviridov
 */
@XmlAccessorType(XmlAccessType.NONE)
public class DependenciesGroup implements Serializable
{

    /**
     * The framework for which the dependency group is intended
     */
    @XmlAttribute(name = "targetFramework")
    @XmlJavaTypeAdapter(TargetFrameworkAdapter.class)
    private Framework targetFramework;

    /**
     * Package dependencies
     */
    @XmlElement(name = "dependency", namespace = Nuspec.NUSPEC_XML_NAMESPACE_2011)
    private List<Dependency> dependencies;

    /**
     * @return package dependencies
     */
    public List<Dependency> getDependencies()
    {
        if (dependencies == null)
        {
            dependencies = new ArrayList<>();
        }

        return dependencies;
    }

    /**
     * @param dependencies
     *            package dependencies
     */
    public void setDependencies(List<Dependency> dependencies)
    {
        this.dependencies = dependencies;
    }

    /**
     * @return the framework for which the dependency group is intended
     */
    public Framework getTargetFramework()
    {
        return targetFramework;
    }

    /**
     * @param targetFramework
     *            the framework for which the dependency group is intended
     */
    public void setTargetFramework(Framework targetFramework)
    {
        this.targetFramework = targetFramework;
    }

    /**
     * Convert list assembly to delimited string
     */
    public static class TargetFrameworkAdapter extends XmlAdapter<String, Framework>
    {

        @Override
        public String marshal(Framework framework)
        {
            if (framework == null)
            {
                return null;
            }

            return framework.name();
        }

        @Override
        public Framework unmarshal(String farmework)
        {
            if (farmework == null)
            {
                return null;
            }

            Framework result = Framework.getByShortName(farmework);
            if (result == null)
            {
                return Framework.getByFullName(farmework);
            }
            else
            {
                return result;
            }
        }
    }
}
