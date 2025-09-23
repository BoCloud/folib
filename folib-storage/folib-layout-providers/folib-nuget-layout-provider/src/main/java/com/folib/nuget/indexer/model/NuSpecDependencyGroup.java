package com.folib.nuget.indexer.model;

import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlElement;
import lombok.Getter;


import java.util.List;


@Getter
public class NuSpecDependencyGroup extends NuSpecGroup {
    @XmlAttribute
    private String targetFramework;

    @XmlElement(
            name = "dependency"
    )
    private List<NuSpecDependency> dependencies;
}
