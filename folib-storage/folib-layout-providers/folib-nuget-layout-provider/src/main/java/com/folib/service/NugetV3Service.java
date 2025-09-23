package com.folib.service;

import com.folib.nuget.filter.NugetSearchRequest;
import com.folib.nugetv3.model.registration.RegistrationResult;
import com.folib.nugetv3.model.registration.RegistrationResultPage;
import com.folib.nugetv3.model.registration.RegistrationResultPageItem;
import com.folib.nugetv3.model.search.SearchResult;
import com.folib.storage.repository.Repository;



public interface NugetV3Service {
    String getType();

    public RegistrationResult getRegistration(Repository repository, String packageId, boolean isSemVer2Endpoint) throws Exception;

    public RegistrationResultPage getRegistrationPage(Repository repository, String packageId, String lower, String upper, boolean isSemVer2Endpoint) throws Exception;

    public RegistrationResultPageItem getRegistrationPageItem(Repository repository, String packageId, String version) throws Exception;

    public int searchCount(NugetSearchRequest searchRequest);

    public SearchResult search(NugetSearchRequest searchRequest) throws Exception;
}
