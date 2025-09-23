package com.folib.nuget.odata.response;

import com.folib.nuget.rest.request.NuGetSearchParameters;


public class CountResponseBuilder extends ODataResponseBuilder{
    public CountResponseBuilder(NuGetSearchParameters nuGetSearchParameters) {
        super(nuGetSearchParameters);
    }


    public long createResponse() {
        return 0;
    }
}
