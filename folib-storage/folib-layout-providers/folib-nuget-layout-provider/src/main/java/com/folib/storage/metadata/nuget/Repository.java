

package com.folib.storage.metadata.nuget;

import jakarta.xml.bind.annotation.XmlAccessType;
import jakarta.xml.bind.annotation.XmlAccessorType;
import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlRootElement;

import java.io.Serializable;

@XmlRootElement(name = "repository", namespace = Nuspec.NUSPEC_XML_NAMESPACE_2011)
@XmlAccessorType(XmlAccessType.NONE)
public class Repository implements Serializable
{

    @XmlAttribute(name = "type")
    private String type;

    @XmlAttribute(name = "url")
    private String url;

    protected String getType()
    {
        return type;
    }

    protected void setType(String type)
    {
        this.type = type;
    }

    protected String getUrl()
    {
        return url;
    }

    protected void setUrl(String url)
    {
        this.url = url;
    }

}
