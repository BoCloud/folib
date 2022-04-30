package com.veadan.folib.providers.layout.p2;

import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import java.util.ArrayList;
import java.util.List;

@XmlRootElement(name = "artifacts")
public class P2Artifacts
{

    private Integer size;

    private List<P2Artifact> artifacts = new ArrayList();

    @XmlElement(name = "artifact")
    public List<P2Artifact> getArtifacts()
    {
        return artifacts;
    }

    public void setArtifacts(List<P2Artifact> artifacts)
    {
        this.artifacts = artifacts;
    }

}
