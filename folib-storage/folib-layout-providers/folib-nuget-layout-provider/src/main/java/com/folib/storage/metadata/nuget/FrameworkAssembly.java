

package com.folib.storage.metadata.nuget;

import jakarta.xml.bind.annotation.XmlAccessType;
import jakarta.xml.bind.annotation.XmlAccessorType;
import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

import java.io.Serializable;
import java.util.EnumSet;

/**
 * A class that describes dependencies on assemblies that come with the .NET
 * framework itself.
 *
 * @author Veadan
 */
@XmlRootElement(name = "frameworkAssembly", namespace = Nuspec.NUSPEC_XML_NAMESPACE_2011)
@XmlAccessorType(XmlAccessType.NONE)
public class FrameworkAssembly implements Serializable
{

    @XmlAttribute(name = "assemblyName")
    private String assemblyName;

    @XmlAttribute(name = "targetFramework")
    @XmlJavaTypeAdapter(AssemblyTargetFrameworkAdapter.class)
    private EnumSet<Framework> targetFrameworks;

    public String getAssemblyName()
    {
        return assemblyName;
    }

    public void setAssemblyName(String assemblyName)
    {
        this.assemblyName = assemblyName;
    }

    /**
     * @return The frameworks for which the assembly is intended
     */
    public EnumSet<Framework> getTargetFrameworks()
    {
        if (targetFrameworks == null)
        {
            targetFrameworks = EnumSet.allOf(Framework.class);
        }
        return targetFrameworks;
    }

    /**
     * @param targetFrameworks
     *            the frameworks for which the assembly is intended
     */
    public void setTargetFrameworks(EnumSet<Framework> targetFrameworks)
    {
        this.targetFrameworks = targetFrameworks;
    }
}
