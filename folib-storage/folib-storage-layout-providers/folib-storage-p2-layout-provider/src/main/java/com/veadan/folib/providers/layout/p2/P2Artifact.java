package com.veadan.folib.providers.layout.p2;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

@XmlRootElement(name = "artifact")
public class P2Artifact
{

    private String version;
    private String id;
    private String classifier;
    private P2Properties properties;

    @XmlAttribute
    public String getVersion()
    {
        return version;
    }

    public void setVersion(String version)
    {
        this.version = version;
    }

    @XmlAttribute
    public String getId()
    {
        return id;
    }

    public void setId(String id)
    {
        this.id = id;
    }

    @XmlAttribute
    public String getClassifier()
    {
        return classifier;
    }

    public void setClassifier(String classifier)
    {
        this.classifier = classifier;
    }

    @XmlElement(name = "properties")
    public P2Properties getProperties()
    {
        return properties;
    }

    public void setProperties(P2Properties properties)
    {
        this.properties = properties;
    }
}
