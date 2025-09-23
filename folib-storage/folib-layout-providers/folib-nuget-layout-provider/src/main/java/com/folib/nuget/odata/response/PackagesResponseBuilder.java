package com.folib.nuget.odata.response;

import com.folib.nuget.odata.feed.Feed;
import com.folib.nuget.rest.request.NuGetSearchParameters;

import javax.ws.rs.core.UriInfo;


public class PackagesResponseBuilder extends ODataResponseBuilder{
    public PackagesResponseBuilder(NuGetSearchParameters nuGetSearchParameters) {
        super(nuGetSearchParameters);
    }

    public Feed createResponse(String title, UriInfo uriInfo, String baseUrl, boolean shouldAddSkipToken) {
        return null;
    }
}
