package com.folib.nuget.indexer.model;

import jakarta.xml.bind.annotation.XmlAccessType;
import jakarta.xml.bind.annotation.XmlAccessorType;
import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlType;
import lombok.Getter;


@Getter
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(
    name = "dependency"
)
public class NuSpecDependency {

    @XmlAttribute
    private String id;
    @XmlAttribute
    private String version;
}