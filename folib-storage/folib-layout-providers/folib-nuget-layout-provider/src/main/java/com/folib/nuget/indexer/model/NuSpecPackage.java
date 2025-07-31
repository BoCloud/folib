package com.folib.nuget.indexer.model;


import jakarta.xml.bind.annotation.*;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
@XmlAccessorType(XmlAccessType.FIELD)
@XmlRootElement(
        name = "package"
)
public class NuSpecPackage {

    @XmlAttribute(
            name = "xmlns"
    )
    private String xmlns;
    @XmlElement(
            name = "metadata"
    )
    private NugetMetadata metadata;
}
