package com.folib.nuget.rest.request;

import cn.hutool.core.builder.HashCodeBuilder;
import jakarta.annotation.Nullable;
import lombok.Getter;
import org.apache.commons.lang.StringUtils;

import javax.annotation.Nonnull;
import javax.ws.rs.core.MultivaluedMap;
import java.util.List;
import java.util.Map;

/**
 * @author LingengMa
 * @date 2025/05/19 19:46
 * @Description:
 */


@Getter
public class NuGetSearchParameters {
    private String filter;
    private String orderBy;
    private String skip;
    private String top;
    private String searchTerm;
    private String targetFramework;
    private String targetFrameworks;
    private String versionConstraints;
    private boolean includePreRelease;
    private String skipToken;
    private String select;
    private String inlinecount;
    private String expand;
    private String id;
    private String packageIds;
    private String versions;
    private boolean includeAllVersions;
    private String semVerLevel;

    public NuGetSearchParameters(NuGetSearchParameters other) {
        this.filter = other.filter;
        this.orderBy = other.orderBy;
        this.skip = other.skip;
        this.top = other.top;
        this.searchTerm = other.searchTerm;
        this.targetFramework = other.targetFramework;
        this.targetFrameworks = other.targetFrameworks;
        this.versionConstraints = other.versionConstraints;
        this.includePreRelease = other.includePreRelease;
        this.skipToken = other.skipToken;
        this.select = other.select;
        this.inlinecount = other.inlinecount;
        this.expand = other.expand;
        this.id = other.id;
        this.packageIds = other.packageIds;
        this.versions = other.versions;
        this.includeAllVersions = other.includeAllVersions;
        this.semVerLevel = other.semVerLevel;
    }


    public NuGetSearchParameters(@Nullable MultivaluedMap<String, String> requestParams, boolean ignoreIsLatestVersionFilter) {
        if (requestParams != null) {
            CaseInsensitiveMap params = new CaseInsensitiveMap();

            for(Map.Entry<String, List<String>> entry : requestParams.entrySet()) {
                params.put(entry.getKey(), entry.getValue().get(0));
            }

            this.updateFields(params);
            this.ignoreFilterIfNeeded(ignoreIsLatestVersionFilter);
        }

    }

    public NuGetSearchParameters(@Nonnull Map<String, String> queryParams, boolean ignoreIsLatestVersionFilter) {
        this.updateFields(queryParams);
        this.ignoreFilterIfNeeded(ignoreIsLatestVersionFilter);
    }

    private void ignoreFilterIfNeeded(boolean ignoreIsLatestVersionFilter) {
        if (ignoreIsLatestVersionFilter && StringUtils.isNotEmpty(this.semVerLevel) && this.semVerLevel.equals("2.0.0") && StringUtils.isNotEmpty(this.filter) && this.filter.equalsIgnoreCase("IsLatestVersion")) {
            this.filter = null;
        }

    }

    private String removeSearchTermQuotes(String searchTerm) {
        if (StringUtils.isNotEmpty(searchTerm) && searchTerm.startsWith("'") && searchTerm.endsWith("'")) {
            searchTerm = StringUtils.removeEnd(StringUtils.removeStart(searchTerm, "'"), "'");
        }

        return searchTerm;
    }



    public String getSearchTerm() {
        return this.removeSearchTermQuotes(this.searchTerm);
    }

    public String getTargetFramework() {
        return this.removeSearchTermQuotes(this.targetFramework);
    }

    public String getTargetFrameworks() {
        return this.removeSearchTermQuotes(this.targetFrameworks);
    }

    public String getVersionConstraints() {
        return this.removeSearchTermQuotes(this.versionConstraints);
    }

    public String getInlineCount() {
        return this.inlinecount;
    }

    public String getId() {
        return this.removeSearchTermQuotes(this.id);
    }

    public String getPackageIds() {
        return this.removeSearchTermQuotes(this.packageIds);
    }

    public String getVersions() {
        return this.removeSearchTermQuotes(this.versions);
    }

    public void resetSelect() {
        this.select = "";
    }

    public void resetSkip() {
        this.skip = "0";
    }

    public String toString() {
        return "filter='" + this.filter + "', orderBy='" + this.orderBy + "', searchTerm='" + this.searchTerm + "', targetFramework='" + this.targetFramework + "', includePrerelease='" + this.includePreRelease + "' skiptoken='" + this.skipToken + "' skip='" + this.skip + "', top='" + this.top + "', select='" + this.select + "', inlinecount='" + this.inlinecount + "', expand='" + this.expand + "' id='" + this.id + "', packageIds='" + this.packageIds + "', versions='" + this.versions + "' includeAllVersions='" + this.includeAllVersions + "'" + ", semVerLevel=" + this.semVerLevel;
    }


    public void appendToUrl(StringBuilder urlBuilder) {
        this.checkAndAppendToUrl(urlBuilder, "$filter", this.filter);
        this.checkAndAppendToUrl(urlBuilder, "$orderby", this.orderBy);
        this.checkAndAppendToUrl(urlBuilder, "searchTerm", this.searchTerm);
        this.checkAndAppendToUrl(urlBuilder, "targetFramework", this.targetFramework);
        this.checkAndAppendToUrl(urlBuilder, "targetFrameworks", this.targetFrameworks);
        this.checkAndAppendToUrl(urlBuilder, "versionConstraints", this.versionConstraints);
        this.checkAndAppendToUrl(urlBuilder, "includePrerelease", Boolean.toString(this.includePreRelease));
        this.checkAndAppendToUrl(urlBuilder, "$skiptoken", this.skipToken);
        this.checkAndAppendToUrl(urlBuilder, "$skip", this.skip);
        this.checkAndAppendToUrl(urlBuilder, "$top", this.top);
        this.checkAndAppendToUrl(urlBuilder, "$select", this.select);
        this.checkAndAppendToUrl(urlBuilder, "$inlinecount", this.inlinecount);
        this.checkAndAppendToUrl(urlBuilder, "$expand", this.expand);
        this.checkAndAppendToUrl(urlBuilder, "id", this.id);
        this.checkAndAppendToUrl(urlBuilder, "packageIds", this.packageIds);
        this.checkAndAppendToUrl(urlBuilder, "versions", this.versions);
        this.checkAndAppendToUrl(urlBuilder, "includeAllVersions", Boolean.toString(this.includeAllVersions));
        this.checkAndAppendToUrl(urlBuilder, "semVerLevel", this.semVerLevel);
    }

    private void checkAndAppendToUrl(StringBuilder urlBuilder, String key, String value) {
        if (StringUtils.isNotBlank(value)) {
            if (urlBuilder.charAt(urlBuilder.length() - 1) != '?') {
                urlBuilder.append("&");
            }

            urlBuilder.append(key).append("=").append(value);
        }

    }

    public int hashCode() {
        return (new HashCodeBuilder(17, 37)).append(this.filter).append(this.orderBy).append(this.searchTerm).append(this.targetFramework).append(this.targetFrameworks).append(this.versionConstraints).append(this.includePreRelease).append(this.select).append(this.inlinecount).append(this.expand).append(this.id).append(this.packageIds).append(this.versions).append(this.includeAllVersions).append(this.semVerLevel).toHashCode();
    }

    public void updateFields(Map<String, String> params) {
        this.filter = (String)params.get("$filter");
        this.orderBy = (String)params.get("$orderby");
        this.skip = (String)params.get("$skip");
        this.top = (String)params.get("$top");
        this.searchTerm = (String)params.get("searchTerm");
        this.targetFramework = (String)params.get("targetFramework");
        this.targetFrameworks = (String)params.get("targetFrameworks");
        this.versionConstraints = (String)params.get("versionConstraints");
        this.includePreRelease = Boolean.parseBoolean((String)params.get("includePrerelease"));
        this.skipToken = (String)params.get("$skiptoken");
        this.select = (String)params.get("$select");
        this.inlinecount = (String)params.get("$inlinecount");
        this.expand = (String)params.get("$expand");
        this.id = (String)params.get("id");
        this.packageIds = (String)params.get("packageIds");
        this.versions = (String)params.get("versions");
        this.includeAllVersions = Boolean.parseBoolean((String)params.get("includeAllVersions"));
        this.semVerLevel = (String)params.get("semVerLevel");
    }
}
