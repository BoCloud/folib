package com.folib.nuget.indexer.model.view;


import com.folib.nuget.indexer.model.NugetMetadata;
import lombok.Getter;

@Getter
public class NugetGeneralInfo {
    private final String iconUrl;
    private final String id;
    private final String pkgTitle;
    private final String version;
    private final String authors;
    private final String owners;
    private final String licenseUrl;
    private final String languages;
    private final boolean requireLicenseAcceptance;
    private final String summary;
    private final String projectUrl;
    private final String description;
    private final String tags;
    private final String releaseNotes;
    private final String copyright;

    public NugetGeneralInfo(NugetMetadata nugetMetadata) {
        this.id = nugetMetadata.getId();
        this.pkgTitle = nugetMetadata.getTitle();
        this.authors = nugetMetadata.getAuthors();
        this.version = nugetMetadata.getVersion();
        this.owners = nugetMetadata.getOwners();
        this.licenseUrl = nugetMetadata.getLicenseUrl();
        this.requireLicenseAcceptance = nugetMetadata.isRequireLicenseAcceptance();
        this.summary = nugetMetadata.getSummary();
        this.tags = nugetMetadata.getTags();
        this.languages = nugetMetadata.getLanguage();
        this.projectUrl = nugetMetadata.getProjectUrl();
        this.iconUrl = nugetMetadata.getIconUrl();
        this.description = nugetMetadata.getDescription();
        this.releaseNotes = nugetMetadata.getReleaseNotes();
        this.copyright = nugetMetadata.getCopyright();
    }
}