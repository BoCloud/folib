package com.veadan.folib.providers.layout.p2;

import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

@XmlRootElement(name = "repository")
public class P2Repository
{

    private P2Artifacts artifacts;

    private P2Mappings mappings;

    @XmlElement(name = "artifacts")
    public P2Artifacts getArtifacts()
    {
        return artifacts;
    }

    public void setArtifacts(P2Artifacts artifacts)
    {
        this.artifacts = artifacts;
    }

    @XmlElement(name = "mappings")
    public P2Mappings getMappings()
    {
        return mappings;
    }

    public void setMappings(P2Mappings mappings)
    {
        this.mappings = mappings;
    }
}
