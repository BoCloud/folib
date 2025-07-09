

package com.folib.storage.metadata.nuget;

import jakarta.xml.bind.annotation.XmlAccessType;
import jakarta.xml.bind.annotation.XmlAccessorType;
import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlRootElement;

import java.io.Serializable;

@XmlRootElement(name = "packageType", namespace = Nuspec.NUSPEC_XML_NAMESPACE_2011)
@XmlAccessorType(XmlAccessType.NONE)
public class PackageType implements Serializable
{

    @XmlAttribute(name = "packageType")
    private String packageType;

    protected String getPackageType()
    {
        return packageType;
    }

    protected void setPackageType(String packageType)
    {
        this.packageType = packageType;
    }

}
