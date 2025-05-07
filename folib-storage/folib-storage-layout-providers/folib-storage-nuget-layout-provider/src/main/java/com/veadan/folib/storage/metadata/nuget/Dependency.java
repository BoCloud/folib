/*
 * Copyright 2019 Veadan Team & Development Ltd.
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

import java.io.Serializable;
import java.util.Objects;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import jakarta.xml.bind.annotation.XmlAccessType;
import jakarta.xml.bind.annotation.XmlAccessorType;
import jakarta.xml.bind.annotation.XmlAttribute;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import static java.text.MessageFormat.format;

/**
 * Description of dependence
 * 
 * @author Unlocker
 */
@XmlAccessorType(XmlAccessType.NONE)
public class Dependency implements Serializable
{

    /**
     * Logger
     */
    private static Logger logger = LoggerFactory.getLogger(Dependency.class);

    /**
     * The format of the string identifier of the package
     */
    private static final String PACKAGE_ID_FORMAT = "[\\w\\.\\-]+";

    /**
     * Dependency line format
     */
    private static final String DEPENDENCY_FORMAT = "^(?<pkgId>" + PACKAGE_ID_FORMAT + ")"
            + ":(?<version>[^:]+)?"
            + ":?(?<frameWork>[^:]+)?$";

    /**
     * Package ID
     */
    @XmlAttribute(name = "id")
    private String id;

    /**
     * Package Version
     */
    public VersionRange versionRange;

    /**
     * Version of the framework for which the dependency is being established
     */
    public Framework framework;

    /**
     * Parses the string representation of a dependency
     *
     * @param dependencyString
     *            string with dependency data
     * @return parsed value
     * @throws NugetFormatException
     *             version format error
     */
    public static Dependency parseString(String dependencyString)
        throws NugetFormatException
    {
        Pattern pattern = Pattern.compile(DEPENDENCY_FORMAT);
        Matcher matcher = pattern.matcher(dependencyString);

        if (!matcher.matches())
        {
            Pattern emptyDependencyPattern = Pattern.compile("^ :: [^:] +");
            final String errorMessage = format("The dependency string does not match the RSS NuGet format: {0}",
                                               dependencyString);
            if (!emptyDependencyPattern.matcher(dependencyString).matches())
            {
                throw new NugetFormatException(errorMessage);
            }

            logger.warn(errorMessage);

            return null;
        }

        Dependency dependency = new Dependency();
        String id = matcher.group("pkgId");
        String versionRangeString = matcher.group("version");
        String frameWorkString = matcher.group("frameWork");

        dependency.id = id;
        dependency.versionRange = VersionRange.parse(versionRangeString);

        if (frameWorkString != null && !frameWorkString.equals(""))
        {
            dependency.framework = Framework.getByShortName(frameWorkString);

            if (dependency.framework == null)
            {
                logger.warn("Package:{} The framework was not found {}", id, frameWorkString);
            }
        }

        return dependency;
    }

    /**
     * @return Package ID
     */
    public String getId()
    {
        return id;
    }

    /**
     * @param id
     *            Package ID
     */
    public void setId(String id)
    {
        this.id = id;
    }

    /**
     * @return version of the package
     */
    public VersionRange getVersionRange()
    {
        return versionRange;
    }

    /**
     * @return string representation of a range of versions
     */
    @XmlAttribute(name = "version")
    public String getVersionRangeString()
    {
        if (versionRange == null)
        {
            return null;
        }
        return versionRange.toString();
    }

    /**
     * @param versionRangeString
     *            string representation of a range of versions
     * @throws NugetFormatException
     *             incorrect version format
     */
    public void setVersionRangeString(String versionRangeString)
        throws NugetFormatException
    {
        this.versionRange = VersionRange.parse(versionRangeString);
    }

    /**
     * @param obj
     *            object to compare with
     * @return true if dependencies are identical
     */
    @Override
    public boolean equals(Object obj)
    {
        if (obj == null)
        {
            return false;
        }
        if (getClass() != obj.getClass())
        {
            return false;
        }
        final Dependency other = (Dependency) obj;
        if (!Objects.equals(this.id, other.id))
        {
            return false;
        }
        
        return Objects.equals(this.versionRange, other.versionRange);
    }

    /**
     * @return HASH object code
     */
    @Override
    public int hashCode()
    {
        int hash = 5;
        hash = 97 * hash + Objects.hashCode(this.id);
        hash = 97 * hash + Objects.hashCode(this.versionRange);
        return hash;
    }

    /**
     * @return string representation of the ID: Range of versions
     * @see VersionRange
     */
    @Override
    public String toString()
    {
        StringBuilder builder = new StringBuilder(128);
        builder.append(id);
        builder.append(":");
        builder.append(versionRange);
        if (framework != null)
        {
            builder.append(":");
            builder.append(framework.name());
        }
        return builder.toString();
    }

}
