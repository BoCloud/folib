package com.folib.controllers.layout.nuget;

import com.folib.artifact.coordinates.NugetCoordinates;
import com.folib.controllers.BaseArtifactController;
import com.folib.nuget.filter.NugetSearchRequest;
import com.folib.nuget.utils.NuGetUrlBuilder;
import com.folib.nugetv3.model.index.NugetServiceIndex;
import com.folib.nugetv3.model.registration.RegistrationResult;
import com.folib.nugetv3.model.registration.RegistrationResultPage;
import com.folib.nugetv3.model.registration.RegistrationResultPageItem;
import com.folib.nugetv3.model.search.SearchResult;
import com.folib.service.NugetServiceFactory;
import com.folib.service.NugetV2Service;
import com.folib.service.NugetV3Service;
import com.folib.storage.repository.Repository;
import com.folib.web.LayoutReqMapping;

import com.folib.web.RepoMapping;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.HttpHeaders;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import javax.inject.Inject;
import javax.ws.rs.core.MediaType;
import java.io.IOException;
import java.net.HttpURLConnection;


@RestController
@LayoutReqMapping(NugetCoordinates.LAYOUT_NAME)
@Api(description = "Nuget_v3坐标控制器", tags = "Nuget_v3坐标控制器")
@Slf4j
public class NugetArtifactV3Controller extends BaseArtifactController {
    @Inject
    private NugetServiceFactory serviceFactory;

    @ApiOperation(value = "获取服务索引")
    @ApiResponses(value = {@ApiResponse(code = HttpURLConnection.HTTP_OK, message = "OK"),
            @ApiResponse(code = HttpURLConnection.HTTP_NOT_FOUND, message = "Repository not found")})
    @GetMapping(path = {"{storageId}/{repositoryId}/api/v3/index.json",
            "{storageId}/{repositoryId}/api/v3/download/symbols"},
            produces = MediaType.APPLICATION_JSON)
    public ResponseEntity index(@RepoMapping Repository repository) throws IOException {
        String repositoryUrl = NuGetUrlBuilder.getNugetRepositoryUrl(repository);
        NugetServiceIndex nugetServiceIndex = new NugetServiceIndex(repositoryUrl);
        return ResponseEntity.ok(nugetServiceIndex);
    }


    @ApiOperation(value = "注册索引, 不包含SemVer2版本")
    @GetMapping(path = "{storageId}/{repositoryId}/api/v3/registration/{lowerId}/index.json",
            produces = MediaType.APPLICATION_JSON)
    public ResponseEntity registration(@RepoMapping Repository repository,
                                       @PathVariable String lowerId) throws IOException {
        NugetV3Service nugetV3Service = serviceFactory.getNugetV3Service(repository);
        try {
            RegistrationResult registrationResult = nugetV3Service.getRegistration(repository, lowerId, false);
            if (registrationResult == null) {
                return ResponseEntity.notFound().build();
            }
            String v3RegistrationBaseUrl = NuGetUrlBuilder.getNugetRepositoryUrl(repository);
            String v2BaseUrl = NuGetUrlBuilder.getNugetV2BaseUrl(repository);
            registrationResult.rewrite(v3RegistrationBaseUrl, v2BaseUrl, lowerId);
            return ResponseEntity.ok(registrationResult);
        } catch (Exception e) {
            log.error("Error fetching registration for packageId: {}", lowerId, e);
            return ResponseEntity.status(HttpURLConnection.HTTP_INTERNAL_ERROR).body("Error fetching registration: " + e.getMessage());
        }
    }


    @ApiOperation(value = "注册页, 不包含SemVer2版本")
    @GetMapping(path = "{storageId}/{repositoryId}/api/v3/registration/{lowerId}/page/{lower}/{upper}.json",
            produces = MediaType.APPLICATION_JSON)
    public ResponseEntity registrationPage(@RepoMapping Repository repository,
                                           @PathVariable String lowerId,
                                           @PathVariable String lower,
                                           @PathVariable String upper) throws IOException {
        lowerId = lowerId.toLowerCase();
        try {
            NugetV3Service nugetV3Service = serviceFactory.getNugetV3Service(repository);
            RegistrationResultPage registrationResultPage = nugetV3Service.getRegistrationPage(repository, lowerId, lower, upper, false);
            String v3RegistrationBaseUrl = NuGetUrlBuilder.getNugetRepositoryUrl(repository);
            String v2BaseUrl = NuGetUrlBuilder.getNugetV2BaseUrl(repository);
            registrationResultPage.rewrite(v3RegistrationBaseUrl, v2BaseUrl, lowerId);
            return ResponseEntity.ok(registrationResultPage);
        } catch (Exception e) {
            log.error("Error fetching registration page for packageId: {}, lower: {}, upper: {}", lowerId, lower, upper, e);
            return ResponseEntity.status(HttpURLConnection.HTTP_INTERNAL_ERROR).body("Error fetching registration page: " + e.getMessage());
        }
    }


    @ApiOperation(value = "获取注册页项")
    @GetMapping(path = {"{storageId}/{repositoryId}/api/v3/registration/{lowerId}/{version}/index.json",
            "{storageId}/{repositoryId}/api/v3/registration-semver2/{lowerId}/{version}/index.json"},
            produces = MediaType.APPLICATION_JSON)
    public ResponseEntity registrationPageItem(@RepoMapping Repository repository,
                                               @PathVariable String lowerId,
                                               @PathVariable String version) throws IOException {
        lowerId = lowerId.toLowerCase();
        NugetV3Service nugetV3Service = serviceFactory.getNugetV3Service(repository);
        try {
            RegistrationResultPageItem registrationResultPageItem = nugetV3Service.getRegistrationPageItem(repository, lowerId, version);
            if (registrationResultPageItem == null) {
                return ResponseEntity.notFound().build();
            }
            String v3RegistrationBaseUrl = NuGetUrlBuilder.getNugetRepositoryUrl(repository);
            String v2BaseUrl = NuGetUrlBuilder.getNugetV2BaseUrl(repository);
            registrationResultPageItem.rewrite(v3RegistrationBaseUrl, v2BaseUrl, lowerId);
            return ResponseEntity.ok(registrationResultPageItem);
        } catch (Exception e) {
            log.error("Error fetching registration page item for packageId: {}, version: {}", lowerId, version, e);
            return ResponseEntity.status(HttpURLConnection.HTTP_INTERNAL_ERROR).body("Error fetching registration page item: " + e.getMessage());
        }
    }


    @ApiOperation(value = "V3检索, TODO")
    @GetMapping(path = "{storageId}/{repositoryId}/api/v3/query",
            produces = MediaType.APPLICATION_JSON)
    public ResponseEntity query(@RepoMapping Repository repository,
                                @RequestParam(value = "q", required = false) String q,
                                @RequestParam(value = "skip", required = false, defaultValue = "0") int skip,
                                @RequestParam(value = "take", required = false, defaultValue = "1000") int take,
                                @RequestParam(value = "prerelease", required = false, defaultValue = "false") boolean includePrerelease,
                                @RequestParam(value = "semVerLevel", required = false, defaultValue = "2.0.0") String semVerLevel
    ) throws IOException {
        boolean shouldRemoveSemver2 = semVerLevel != null && !semVerLevel.equals("2.0.0");
        NugetSearchRequest searchRequest = NugetSearchRequest.builder()
                .repository(repository)
                .searchTerm(q)
                .includePreRelease(includePrerelease)
                .skip(skip)
                .take(take)
                .shouldRemoveSemver2(shouldRemoveSemver2)
                .build();
        NugetV3Service nugetV3Service = serviceFactory.getNugetV3Service(repository);
        try {
            SearchResult searchResult = nugetV3Service.search(searchRequest);
            if (searchResult == null) {
                return ResponseEntity.notFound().build();
            }
            String v3RegistrationBaseUrl = NuGetUrlBuilder.getNugetRepositoryUrl(repository);
            String v2BaseUrl = NuGetUrlBuilder.getNugetV2BaseUrl(repository);
            searchResult.rewrite(v3RegistrationBaseUrl, v2BaseUrl);
            return ResponseEntity.ok(searchResult);
        } catch (Exception e) {
            log.error("Error during Nuget V3 query: {}", e.getMessage(), e);
            return ResponseEntity.status(HttpURLConnection.HTTP_INTERNAL_ERROR).body("Error during query: " + e.getMessage());
        }
    }


    @ApiOperation(value = "注册索引, 包含SemVer2版本")
    @GetMapping(path = "{storageId}/{repositoryId}/api/v3/registration-semver2/{lowerId}/index.json",
            produces = MediaType.APPLICATION_JSON)
    public ResponseEntity registrationSemVer2(@RepoMapping Repository repository,
                                              @PathVariable String lowerId) throws IOException {
        lowerId = lowerId.toLowerCase();
        NugetV3Service nugetV3Service = serviceFactory.getNugetV3Service(repository);
        try {
            RegistrationResult registrationResult = nugetV3Service.getRegistration(repository, lowerId, true);
            if (registrationResult == null) {
                return ResponseEntity.notFound().build();
            }
            String v3RegistrationBaseUrl = NuGetUrlBuilder.getNugetRepositoryUrl(repository);
            String v2BaseUrl = NuGetUrlBuilder.getNugetV2BaseUrl(repository);
            registrationResult.rewrite(v3RegistrationBaseUrl, v2BaseUrl, lowerId);
            return ResponseEntity.ok(registrationResult);
        } catch (Exception e) {
            log.error("Error fetching SemVer2 registration for packageId: {}", lowerId, e);
            return ResponseEntity.status(HttpURLConnection.HTTP_INTERNAL_ERROR).body("Error fetching SemVer2 registration: " + e.getMessage());
        }
    }


    @ApiOperation(value = "注册页, 包含SemVer2版本")
    @GetMapping(path = "{storageId}/{repositoryId}/api/v3/registration-semver2/{lowerId}/page/{lower}/{upper}.json",
            produces = MediaType.APPLICATION_JSON)
    public ResponseEntity registrationPageSemver2(@RepoMapping Repository repository,
                                                  @PathVariable String lowerId,
                                                  @PathVariable String lower,
                                                  @PathVariable String upper) throws IOException {
        lowerId = lowerId.toLowerCase();
        try {
            NugetV3Service nugetV3Service = serviceFactory.getNugetV3Service(repository);
            RegistrationResultPage registrationResultPage = nugetV3Service.getRegistrationPage(repository, lowerId, lower, upper, true);
            String v3RegistrationBaseUrl = NuGetUrlBuilder.getNugetRepositoryUrl(repository);
            String v2BaseUrl = NuGetUrlBuilder.getNugetV2BaseUrl(repository);
            registrationResultPage.rewrite(v3RegistrationBaseUrl, v2BaseUrl, lowerId);
            return ResponseEntity.ok(registrationResultPage);
        } catch (Exception e) {
            log.error("Error fetching SemVer2 registration page for packageId: {}, lower: {}, upper: {}", lowerId, lower, upper, e);
            return ResponseEntity.status(HttpURLConnection.HTTP_INTERNAL_ERROR).body("Error fetching SemVer2 registration page: " + e.getMessage());
        }
    }


    @ApiOperation(value = "index2.txt")
    @GetMapping(path = {"{storageId}/{repositoryId}/api/v3/symbols",
            "{storageId}/{repositoryId}/api/v3/symbols/index2.txt"})
    public ResponseEntity getSymbolIndex2(@RepoMapping Repository repository)
            throws Exception {
        return ResponseEntity.ok("This is a symbol server.");
    }


    @ApiOperation(value = "下载符号包pdb")
    @GetMapping(path = {"{storageId}/{repositoryId}/api/v3/symbols/{packageId}.pdb/{guid}/{packageId}.pdb",
            "{storageId}/{repositoryId}/api/v3/symbols/{shortId}/{packageId}.pdb/{guid}/{packageId}.pdb"})
    public void getSymbolPdb(@RepoMapping Repository repository,
                             @PathVariable("packageId") String packageId,
                             @PathVariable("guid") String guid,
                             @RequestHeader HttpHeaders httpHeaders,
                             HttpServletRequest request,
                             HttpServletResponse response)
            throws Exception {
        if (guid.endsWith("ffffffff")) {
            guid = guid.substring(0, guid.length() - 8);
        }
        NugetV2Service nugetV2Service = serviceFactory.getNugetV2Service(repository);
        nugetV2Service.provideDownloadSymbolPdb(repository, packageId, guid, response, httpHeaders);
    }

}
