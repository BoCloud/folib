

package com.folib.storage.metadata.nuget.rss;


import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlValue;

@XmlRootElement(name = "title", namespace = PackageFeed.ATOM_XML_NAMESPACE)
public class Title
{

    @XmlAttribute(name = "type")
    private String type = "text";

    @XmlValue
    public String value;

    public Title()
    {
    }

    public Title(String value)
    {
        this.value = value;
    }
}
