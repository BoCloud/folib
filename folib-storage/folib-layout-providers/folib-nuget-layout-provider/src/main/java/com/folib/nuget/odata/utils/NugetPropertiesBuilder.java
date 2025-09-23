package com.folib.nuget.odata.utils;

import com.folib.nuget.indexer.model.NugetMetadata;
import com.folib.nuget.odata.feed.Properties;
import com.folib.nuget.utils.NugetSemVerUtil;



public class NugetPropertiesBuilder {
    public static Properties buildProperties(NugetMetadata nugetMetadata) {
        Properties properties = new Properties();
        properties.setId(nugetMetadata.getId());
//        properties.setLastUpdated();
        properties.setVersion(nugetMetadata.getVersion());
//        properties.setNormalizedVersion();
//        properties.setLastEdited();
//        properties.setLicenseReportUrl();
//        properties.setLicenseNames();
        properties.setCopyright(nugetMetadata.getCopyright());
//        properties.setCreated();
//        properties.setDependencies(nugetMetadata.getDependencies());
        properties.setDescription(nugetMetadata.getDescription());
//        properties.setDownloadCount();
//        properties.setGalleryDetailsUrl();
        properties.setIconUrl(nugetMetadata.getIconUrl());
//        properties.setIsLatestVersion();
//        properties.setIsAbsoluteLatestVersion();
        properties.setPreRelease(NugetSemVerUtil.isPreReleaseVersion(nugetMetadata.getVersion()));
        properties.setLanguage(nugetMetadata.getLanguage());
//        properties.setPublished();
        properties.setLicenseUrl(nugetMetadata.getLicenseUrl());
//        properties.setPackageHash();
//        properties.setPackageHashAlgorithm();
//        properties.setPackageSize();
        properties.setProjectUrl(nugetMetadata.getProjectUrl());
//        properties.setReportAbuseUrl();
        properties.setReleaseNotes(nugetMetadata.getReleaseNotes());
        properties.setRequireLicenseAcceptance(nugetMetadata.isRequireLicenseAcceptance());
        properties.setTags(nugetMetadata.getTags());
        properties.setTitle(nugetMetadata.getTitle());
//        properties.setVersionDownloadCount();
        properties.setAuthors(nugetMetadata.getAuthors());
//        properties.setMinClientVersion();
        properties.setSummary(nugetMetadata.getSummary());
        return properties;
    }
}
