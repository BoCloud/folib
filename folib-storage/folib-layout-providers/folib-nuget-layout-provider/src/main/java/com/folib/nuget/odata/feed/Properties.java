package com.folib.nuget.odata.feed;

import com.folib.nuget.utils.NugetV2Util;
import com.folib.nuget.utils.jaxb.AttributedBoolean;
import com.folib.nuget.utils.jaxb.AttributedDateTime;
import com.folib.nuget.utils.jaxb.AttributedInt64;
import com.folib.nuget.utils.jaxb.AttributedTextNull;
import jakarta.annotation.Nullable;
import jakarta.xml.bind.annotation.XmlAccessType;
import jakarta.xml.bind.annotation.XmlAccessorType;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlType;
import lombok.Data;
import lombok.NoArgsConstructor;


import java.io.Serializable;
import java.time.LocalDateTime;
import java.time.ZoneOffset;
import java.time.format.DateTimeFormatter;

/**
 * @author LingengMa
 * @date 2025/05/19 16:17
 * @Description:
 */


@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(
        namespace = "http://schemas.microsoft.com/ado/2007/08/dataservices"
)
@Data
@NoArgsConstructor
public class Properties implements Serializable {
    @XmlElement(
            name = "Id"
    )
    private String id;
    private String lowerCaseId;
    @XmlElement(
            name = "lastUpdated"
    )
    private String lastUpdated;
    @XmlElement(
            name = "Version"
    )
    private String version;
    @XmlElement(
            name = "NormalizedVersion"
    )
    private String normalizedVersion;
    @XmlElement(
            name = "LastEdited"
    )
    private AttributedDateTime lastEdited;
    @XmlElement(
            name = "LicenseReportUrl"
    )
    private AttributedTextNull licenseReportUrl;
    @XmlElement(
            name = "LicenseNames"
    )
    private AttributedTextNull licenseNames;
    @XmlElement(
            name = "Copyright"
    )
    private AttributedTextNull copyright;
    @XmlElement(
            name = "Created"
    )
    private AttributedDateTime created;
    @XmlElement(
            name = "Dependencies"
    )
    private String dependencies;
    @XmlElement(
            name = "Description"
    )
    private String description;
    @XmlElement(
            name = "DownloadCount"
    )
    private AttributedInt64 downloadCount;
    @XmlElement(
            name = "GalleryDetailsUrl"
    )
    private String galleryDetailsUrl;
    @XmlElement(
            name = "IconUrl"
    )
    private AttributedTextNull iconUrl;
    @XmlElement(
            name = "IsLatestVersion"
    )
    private AttributedBoolean isLatestVersion;
    @XmlElement(
            name = "IsAbsoluteLatestVersion"
    )
    private AttributedBoolean isAbsoluteLatestVersion;
    @XmlElement(
            name = "IsPrerelease"
    )
    private AttributedBoolean isPreRelease = new AttributedBoolean();
    @XmlElement(
            name = "Language"
    )
    @Nullable
    private String language;
    @XmlElement(
            name = "Published"
    )
    private AttributedDateTime published;
    @XmlElement(
            name = "LicenseUrl"
    )
    private String licenseUrl;
    @XmlElement(
            name = "PackageHash"
    )
    private String packageHash;
    @XmlElement(
            name = "PackageHashAlgorithm"
    )
    private String packageHashAlgorithm;
    @XmlElement(
            name = "PackageSize"
    )
    private AttributedInt64 packageSize;
    @XmlElement(
            name = "ProjectUrl"
    )
    private String projectUrl;
    @XmlElement(
            name = "ReportAbuseUrl"
    )
    private String reportAbuseUrl;
    @XmlElement(
            name = "ReleaseNotes"
    )
    private AttributedTextNull releaseNotes;
    @XmlElement(
            name = "RequireLicenseAcceptance"
    )
    private AttributedBoolean requireLicenseAcceptance;
    @XmlElement(
            name = "Tags"
    )
    private String tags;
    @XmlElement(
            name = "Title"
    )
    @Nullable
    private String title;
    @XmlElement(
            name = "VersionDownloadCount"
    )
    private AttributedInt64 versionDownloadCount = new AttributedInt64();
    @XmlElement(
            name = "Authors"
    )
    private String authors;
    @XmlElement(
            name = "MinClientVersion"
    )
    private AttributedTextNull minClientVersion = new AttributedTextNull();
    @XmlElement(
            name = "Summary"
    )
    private String summary;


    public void setId(String id) {
        this.id = id;
        this.lowerCaseId = id.toLowerCase();
    }

    public LocalDateTime getLastEdited() {
        return this.lastEdited == null ? null : this.lastEdited.getDateTime();
    }

    public void setLastEdited(LocalDateTime lastEdited) {
        this.lastEdited = new AttributedDateTime(lastEdited.format(DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm:ss.SSSZZ")));
    }

    public String getLicenseReportUrl() {
        return this.licenseReportUrl == null ? null : this.licenseReportUrl.getValue();
    }

    public void setLicenseReportUrl(String licenseReportUrl) {
        this.licenseReportUrl = new AttributedTextNull(licenseReportUrl);
    }

    public String getLicenseNames() {
        return this.licenseNames == null ? null : this.licenseNames.getValue();
    }

    public void setLicenseNames(String licenseNames) {
        this.licenseNames = new AttributedTextNull(licenseNames);
    }

    public String getCopyright() {
        return this.copyright == null ? null : this.copyright.getValue();
    }

    public void setCopyright(String copyright) {
        this.copyright = new AttributedTextNull(copyright);
    }

    public long getPackageSize() {
        return this.packageSize == null ? 0L : this.packageSize.getValue();
    }

    public void setPackageSize(long packageSize) {
        this.packageSize = new AttributedInt64(packageSize);
    }

    public boolean isRequireLicenseAcceptance() {
        return this.requireLicenseAcceptance == null ? false : this.requireLicenseAcceptance.getValue();
    }

    public void setRequireLicenseAcceptance(boolean requireLicenseAcceptance) {
        this.requireLicenseAcceptance = new AttributedBoolean(requireLicenseAcceptance);
    }

    public boolean isLatestVersion() {
        return this.isLatestVersion == null ? false : this.isLatestVersion.getValue();
    }

    public void setLatestVersion(boolean latestVersion) {
        this.isLatestVersion = new AttributedBoolean(latestVersion);
    }

    public boolean isAbsoluteLatestVersion() {
        return this.isAbsoluteLatestVersion == null ? false : this.isAbsoluteLatestVersion.getValue();
    }

    public void setAbsoluteLatestVersion(boolean absoluteLatestVersion) {
        this.isAbsoluteLatestVersion = new AttributedBoolean(absoluteLatestVersion);
    }

    public String getReleaseNotes() {
        return this.releaseNotes == null ? null : this.releaseNotes.getValue();
    }

    public void setReleaseNotes(String releaseNotes) {
        this.releaseNotes = new AttributedTextNull(releaseNotes);
    }

    public boolean isPreRelease() {
        return this.isPreRelease == null ? false : this.isPreRelease.getValue();
    }

    public void setPreRelease(boolean preRelease) {
        this.isPreRelease = new AttributedBoolean(preRelease);
    }

    public long getVersionDownloadCount() {
        return this.versionDownloadCount == null ? 0L : this.versionDownloadCount.getValue();
    }

    public void setVersionDownloadCount(long versionDownloadCount) {
        this.versionDownloadCount = new AttributedInt64(versionDownloadCount);
    }

    public String getCreated() {
        return this.created == null ? null : this.created.getValue();
    }

    public void setCreated(String created) {
        this.created = new AttributedDateTime(created);
    }

    public void setCreatedTime(long createdTime) {
        this.created = new AttributedDateTime(NugetV2Util.PACKAGE_ENTRY_DATE_FORMAT.format(LocalDateTime.ofEpochSecond(createdTime / 1000, 0, ZoneOffset.UTC)));
    }

    public void setLastUpdatedTime(long lastUpdatedTime) {
        this.lastUpdated = NugetV2Util.PACKAGE_ENTRY_DATE_FORMAT.format(LocalDateTime.ofEpochSecond(lastUpdatedTime / 1000, 0, ZoneOffset.UTC));
    }

    public String getPublished() {
        return this.published == null ? null : this.published.getValue();
    }

    public void setPublished(String published) {
        this.published = new AttributedDateTime(published);
    }

    public void setPublishedTime(long publishedTime) {
        this.published = new AttributedDateTime(NugetV2Util.PACKAGE_ENTRY_DATE_FORMAT.format(LocalDateTime.ofEpochSecond(publishedTime / 1000, 0, ZoneOffset.UTC)));
    }

    public String getIconUrl() {
        return this.iconUrl == null ? null : this.iconUrl.getValue();
    }

    public void setIconUrl(String iconUrl) {
        this.iconUrl = new AttributedTextNull(iconUrl);
    }

    public long getDownloadCount() {
        return this.downloadCount == null ? 0L : this.downloadCount.getValue();
    }

    public void setDownloadCount(long downloadCount) {
        this.downloadCount = new AttributedInt64(downloadCount);
    }

    public String getMinClientVersion() {
        return this.minClientVersion == null ? null : this.minClientVersion.getValue();
    }
}
