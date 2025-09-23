package com.folib.nuget.odata.response;

import com.folib.nuget.odata.feed.Entry;
import com.folib.nuget.rest.request.NuGetSearchParameters;
import lombok.extern.slf4j.Slf4j;




@Slf4j
public class PackageEntryResponseBuilder extends ODataResponseBuilder{
    public PackageEntryResponseBuilder(NuGetSearchParameters nuGetSearchParameters) {
        super(nuGetSearchParameters);
    }

    public Entry createResponse(String id, String baseUrl) {
        return null;
    }
}
