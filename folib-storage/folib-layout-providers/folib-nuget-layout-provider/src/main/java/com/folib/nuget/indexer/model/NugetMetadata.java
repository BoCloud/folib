package com.folib.nuget.indexer.model;

import jakarta.xml.bind.annotation.*;
import lombok.Getter;

import java.util.List;


@Getter
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(
    name = "metadata"
)
public class NugetMetadata {

    private String id;
    private String version;
    private String title;
    private String authors;
    private String owners;
    private String licenseUrl;
    private String projectUrl;
    private String iconUrl;
    private boolean requireLicenseAcceptance;
    private String description;
    private String summary;
    private String language;
    private String tags;
    private String releaseNotes;
    private String copyright;
    @XmlElementWrapper(
            name = "dependencies"
    )
    @XmlElements({@XmlElement(
            name = "group",
            type = NuSpecDependencyGroup.class
    ), @XmlElement(
            name = "dependency",
            type = NuSpecDependency.class
    )})
    private List dependencies;
    @XmlElementWrapper(
            name = "references"
    )
    @XmlElements({@XmlElement(
            name = "group",
            type = NuSpecReferenceGroup.class
    ), @XmlElement(
            name = "reference",
            type = NuSpecReference.class
    )})
    private List references;
    @XmlElementWrapper(
            name = "frameworkAssemblies"
    )
    @XmlElement(
            name = "frameworkAssembly"
    )
    private List<NuSpecFrameWorkAssembly> frameworkAssemblies;
}
