package com.folib.nuget.indexer.model;

import lombok.Data;

import java.util.Collection;
import java.util.Map;


@Data
public class NugetArtifactProperties {

    private Collection<Map<String, Collection<String>>> properties;
}
