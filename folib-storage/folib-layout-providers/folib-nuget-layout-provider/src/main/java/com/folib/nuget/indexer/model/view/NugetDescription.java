package com.folib.nuget.indexer.model.view;


import com.folib.nuget.indexer.model.NugetMetadata;
import lombok.Getter;

@Getter
public class NugetDescription {
    private String description;

    public NugetDescription(NugetMetadata nuMetaData) {
        this.description = nuMetaData.getDescription();
    }
}
