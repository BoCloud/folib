package com.folib.service;

import com.folib.nuget.filter.NugetSearchRequest;
import com.folib.nuget.odata.feed.Entry;
import com.folib.nuget.odata.feed.Feed;
import com.folib.storage.repository.Repository;
import jakarta.servlet.http.HttpServletResponse;
import org.springframework.http.HttpHeaders;




public interface NugetV2Service {
    String getType();

    Feed search(NugetSearchRequest searchRequest) throws Exception;

    Feed findPackageById(Repository repository, String packageId, boolean isSemVer2Endpoint) throws Exception;

    Entry packageEntry(Repository repository, String packageId, String version) throws Exception;

    void provideDownloadNupkg(Repository repository, String packageId, String version, HttpServletResponse response) throws Exception;

    void provideDownloadSymbolPdb(Repository repository, String packageId, String guid, HttpServletResponse response, HttpHeaders httpHeaders) throws Exception;

    void provideDownloadSnupkg(Repository repository, String packageId, String version, HttpServletResponse response) throws Exception;

    int searchCount(NugetSearchRequest searchRequest);
//    int findPackageByIdCount(Repository repository, String packageId) throws Exception;

}
