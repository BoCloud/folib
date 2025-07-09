

package com.folib.storage.metadata.nuget;


import jakarta.xml.bind.annotation.XmlAccessType;
import jakarta.xml.bind.annotation.XmlAccessorType;
import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlRootElement;

import java.io.Serializable;

@XmlRootElement(name = "file", namespace = Nuspec.NUSPEC_XML_NAMESPACE_2011)
@XmlAccessorType(XmlAccessType.NONE)
public class File implements Serializable
{

    @XmlAttribute(name = "src")
    private String src;

    @XmlAttribute(name = "target")
    private String target;

    @XmlAttribute(name = "exclude")
    private String exclude;

    protected String getSrc()
    {
        return src;
    }

    protected void setSrc(String src)
    {
        this.src = src;
    }

    protected String getTarget()
    {
        return target;
    }

    protected void setTarget(String target)
    {
        this.target = target;
    }

    protected String getExclude()
    {
        return exclude;
    }

    protected void setExclude(String exclude)
    {
        this.exclude = exclude;
    }

}
