package com.folib.nuget.odata.response;

import com.folib.nuget.rest.request.NuGetSearchParameters;

import java.util.HashMap;
import java.util.Map;




public abstract class ODataResponseBuilder {
    protected NuGetSearchParameters nuGetSearchParameters;

    public ODataResponseBuilder(NuGetSearchParameters nuGetSearchParameters) {
        this.nuGetSearchParameters = nuGetSearchParameters;
    }

    protected Map<String, String> buildCustomOptions() {
        Map<String, String> options = new HashMap();
        options.put("searchTerm", this.nuGetSearchParameters.getSearchTerm());
        options.put("targetFramework", this.nuGetSearchParameters.getTargetFramework());
        options.put("targetFrameworks", this.nuGetSearchParameters.getTargetFrameworks());
        options.put("versionConstraints", this.nuGetSearchParameters.getVersionConstraints());
        options.put("includePrerelease", String.valueOf(this.nuGetSearchParameters.isIncludePreRelease()));
        options.put("id", this.nuGetSearchParameters.getId());
        options.put("includeAllVersions", String.valueOf(this.nuGetSearchParameters.isIncludeAllVersions()));
        return options;
    }

    protected void alterQueryParams() {
    }
}
