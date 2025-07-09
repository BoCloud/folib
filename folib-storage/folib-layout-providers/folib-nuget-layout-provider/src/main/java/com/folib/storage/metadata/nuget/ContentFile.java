


package com.folib.storage.metadata.nuget;

import jakarta.xml.bind.annotation.XmlAccessType;
import jakarta.xml.bind.annotation.XmlAccessorType;
import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlRootElement;

import java.io.Serializable;

@XmlRootElement(name = "files", namespace = Nuspec.NUSPEC_XML_NAMESPACE_2011)
@XmlAccessorType(XmlAccessType.NONE)
public class ContentFile implements Serializable
{

    @XmlAttribute(name = "include")
    private String include;

    @XmlAttribute(name = "exclude")
    private String exclude;

    @XmlAttribute(name = "buildAction")
    private String buildAction;

    @XmlAttribute(name = "copyToOutput")
    private Boolean copyToOutput;

    @XmlAttribute(name = "flatten")
    private Boolean flatten;

    protected String getInclude()
    {
        return include;
    }

    protected void setInclude(String include)
    {
        this.include = include;
    }

    protected String getBuildAction()
    {
        return buildAction;
    }

    protected void setBuildAction(String buildAction)
    {
        this.buildAction = buildAction;
    }

    protected Boolean getCopyToOutput()
    {
        return copyToOutput;
    }

    protected void setCopyToOutput(Boolean copyToOutput)
    {
        this.copyToOutput = copyToOutput;
    }

    protected String getExclude()
    {
        return exclude;
    }

    protected void setExclude(String exclude)
    {
        this.exclude = exclude;
    }

    protected Boolean getFlatten()
    {
        return flatten;
    }

    protected void setFlatten(Boolean flatten)
    {
        this.flatten = flatten;
    }

}
