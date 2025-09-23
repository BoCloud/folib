package com.folib.nuget.indexer.model;


import jakarta.xml.bind.annotation.XmlElement;
import lombok.Getter;

import java.util.List;

@Getter
public class NuSpecReferenceGroup extends NuSpecGroup {
    @XmlElement(
            name = "reference"
    )
    private List<NuSpecReference> references;
}
