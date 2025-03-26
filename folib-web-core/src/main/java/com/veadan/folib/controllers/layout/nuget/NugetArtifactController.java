package com.veadan.folib.controllers.layout.nuget;

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.Writer;
import java.net.HttpURLConnection;
import java.net.URI;
import java.nio.file.Files;
import java.nio.file.Path;
import java.security.NoSuchAlgorithmException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.stream.Collectors;

import javax.inject.Inject;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.ws.rs.core.MediaType;
import javax.xml.bind.JAXBException;

import com.veadan.folib.annotation.AuditLog;
import com.veadan.folib.artifact.coordinates.versioning.SemanticVersion;
import com.veadan.folib.data.criteria.Paginator;
import com.veadan.folib.data.criteria.Predicate;
import com.veadan.folib.enums.AuditEventNameEnum;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.services.ArtifactTagService;
import com.veadan.folib.artifact.ArtifactTag;
import com.veadan.folib.artifact.coordinates.NugetArtifactCoordinates;
import com.veadan.folib.artifact.coordinates.PathNupkg;
import com.veadan.folib.controllers.BaseArtifactController;
import com.veadan.folib.domain.Artifact;
import com.veadan.folib.domain.ArtifactTagEntity;
import com.veadan.folib.nuget.NugetSearchRequest;
import com.veadan.folib.nuget.filter.NugetODataFilterQueryParser;
import com.veadan.folib.providers.repository.RepositoryProvider;
import com.veadan.folib.providers.repository.RepositoryProviderRegistry;
import com.veadan.folib.providers.repository.RepositorySearchRequest;
import com.veadan.folib.repository.NugetRepositoryFeatures.RepositorySearchEventListener;
import com.veadan.folib.storage.metadata.nuget.NugetFormatException;
import com.veadan.folib.storage.metadata.nuget.Nupkg;
import com.veadan.folib.storage.metadata.nuget.Nuspec;
import com.veadan.folib.storage.metadata.nuget.TempNupkgFile;
import com.veadan.folib.storage.metadata.nuget.rss.EntryProperties;
import com.veadan.folib.storage.metadata.nuget.rss.PackageEntry;
import com.veadan.folib.storage.metadata.nuget.rss.PackageFeed;
import com.veadan.folib.storage.repository.Repository;
import com.veadan.folib.web.LayoutRequestMapping;
import com.veadan.folib.web.RepositoryMapping;
import io.swagger.annotations.*;
import org.springframework.core.io.InputStreamResource;
import org.springframework.core.io.Resource;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.context.request.RequestContextHolder;
import org.springframework.web.context.request.ServletRequestAttributes;
import org.springframework.web.multipart.MultipartFile;

/**
 * This Controller used to handle Nuget requests.
 * 
 * @author @author veadan
 *
 */
@RestController
@LayoutRequestMapping(NugetArtifactCoordinates.LAYOUT_NAME)
@Api(description = "Nuget坐标控制器",tags = "Nuget坐标控制器")
public class NugetArtifactController
        extends BaseArtifactController
{

    @Inject
    private ArtifactTagService artifactTagService;

    @Inject
    private RepositoryProviderRegistry repositoryProviderRegistry;

    @Inject
    private RepositorySearchEventListener repositorySearchEventListener;

    @DeleteMapping(path = { "{storageId}/{repositoryId}/{packageId}/{version}" })
    @PreAuthorize("hasAuthority('ARTIFACTS_DEPLOY')")
    public ResponseEntity deletePackage(@RequestHeader(name = "X-NuGet-ApiKey", required = false) String apiKey,
                                        @RepositoryMapping Repository repository,
                                        @PathVariable("packageId") String packageId,
                                        @PathVariable("version") String version)
    {
        final String storageId = repository.getStorage().getId();
        final String repositoryId = repository.getId();

        logger.info("Nuget delete request: storageId-[{}]; repositoryId-[{}]; packageId-[{}]", storageId, repositoryId,
                    packageId);

        RepositoryPath path = repositoryPathResolver.resolve(storageId, repositoryId, String.format("%s/%s/%s.nuspec", packageId, version, packageId));;
        try
        {
            //TODO: we should move associated files deletion into corresponding layout providers.
            artifactManagementService.delete(path, true);
            path = repositoryPathResolver.resolve(storageId, repositoryId, String.format("%s/%s/%s.%s.nupkg", packageId, version, packageId,version));
            artifactManagementService.delete(path, true);
            path = repositoryPathResolver.resolve(storageId, repositoryId, String.format("%s/%s/%s.%s.nupkg.sha512", packageId, version, packageId,version));
            artifactManagementService.delete(path, true);
        }
        catch (IOException e)
        {
            logger.error("Failed to process Nuget delete request: path-[{}]", path, e);

            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).body(e.getMessage());
        }
        
        return ResponseEntity.status(HttpStatus.OK).build();
    }
    
    @GetMapping(path = { "{storageId}/{repositoryId}/Search()/$count" }, produces = MediaType.TEXT_PLAIN)
    public ResponseEntity<String> countPackages(@RepositoryMapping Repository repository,
                                                @RequestParam(name = "$filter", required = false) String filter,
                                                @RequestParam(name = "searchTerm", required = false) String searchTerm,
                                                @RequestParam(name = "targetFramework", required = false) String targetFramework)
    {
        final String storageId = repository.getStorage().getId();
        final String repositoryId = repository.getId();

        String normalizedSearchTerm = normaliseSearchTerm(searchTerm);
        
        NugetSearchRequest nugetSearchRequest = new NugetSearchRequest();
        nugetSearchRequest.setFilter(filter);
        nugetSearchRequest.setSearchTerm(searchTerm);
        nugetSearchRequest.setTargetFramework(targetFramework);
        repositorySearchEventListener.setNugetSearchRequest(nugetSearchRequest);

        RepositoryProvider provider = repositoryProviderRegistry.getProvider(repository.getType());
        
        RepositorySearchRequest predicate = createSearchPredicate(filter, normalizedSearchTerm);
        Long count = provider.count(storageId, repositoryId, predicate);

        return new ResponseEntity<>(String.valueOf(count), HttpStatus.OK);
    }

    @GetMapping(path = { "{storageId}/{repositoryId}/{searchCommandName:(?:Packages(?:\\(\\))?|Search\\(\\))}" },
                produces = MediaType.APPLICATION_XML)
    public ResponseEntity<?> searchPackages(@RepositoryMapping Repository repository,
                                            @PathVariable(name = "searchCommandName") String searchCommandName,
                                            @RequestParam(name = "$filter", required = false) String filter,
                                            @RequestParam(name = "$orderby", required = false, defaultValue = "Id") String orderBy,
                                            @RequestParam(name = "$skip", required = false) Long skip,
                                            @RequestParam(name = "$top", required = false) Integer top,
                                            @RequestParam(name = "searchTerm", required = false) String searchTerm,
                                            @RequestParam(name = "targetFramework", required = false) String targetFramework,
                                            HttpServletResponse response)
            throws JAXBException, IOException
    {
        final String storageId = repository.getStorage().getId();
        final String repositoryId = repository.getId();

        String normalizedSearchTerm = normaliseSearchTerm(searchTerm);
        
        NugetSearchRequest nugetSearchRequest = new NugetSearchRequest();
        nugetSearchRequest.setFilter(filter);
        nugetSearchRequest.setSearchTerm(searchTerm);
        nugetSearchRequest.setTargetFramework(targetFramework);
        repositorySearchEventListener.setNugetSearchRequest(nugetSearchRequest);
        
        String feedId = getFeedUri(((ServletRequestAttributes) RequestContextHolder.currentRequestAttributes()).getRequest(),
                                   storageId,
                                   repositoryId);

        Collection<? extends Nupkg> files;
        files = getPackages(repository,
                            filter,
                            orderBy,
                            normalizedSearchTerm,
                            targetFramework,
                            skip,
                            top);

        PackageFeed feed = transform(feedId, files);

        response.setHeader(HttpHeaders.CONTENT_TYPE, MediaType.APPLICATION_XML);
        feed.writeXml(response.getOutputStream());

        return new ResponseEntity<>(HttpStatus.OK);
    }
    
    private PackageFeed transform(String feedId,
                                  Collection<? extends Nupkg> files)
    {
        PackageFeed feed = new PackageFeed();
        // feed.setId(getContext().getRootUri().toString());
        feed.setId(feedId);
        feed.setUpdated(new Date());
        feed.setTitle("Packages");
        List<PackageEntry> packageEntrys = new ArrayList<>();
        for (Nupkg nupkg : files)
        {
            try
            {
                PackageEntry entry = createPackageEntry(feedId, (PathNupkg) nupkg);
                calculateFeedEntryProperties((PathNupkg) nupkg, entry.getProperties());
                packageEntrys.add(entry);
            }
            catch (NoSuchAlgorithmException | IOException | NugetFormatException e)
            {
                logger.error("Failed to parse package {}", nupkg, e);
            }
        }
        logger.info("Got {} packages", new Object[] { packageEntrys.size() });
        feed.setEntries(packageEntrys);
        return feed;
    }

    private void calculateFeedEntryProperties(PathNupkg nupkg,
                                              EntryProperties properties) throws IOException
    {
        RepositoryPath path = nupkg.getPath();
        Artifact artifactEntry = path.getArtifactEntry();

        properties.setId(nupkg.getId());

        properties.setReportAbuseUrl("");

        properties.setDownloadCount(artifactEntry.getDownloadCount());
        properties.setVersionDownloadCount(artifactEntry.getDownloadCount());

        properties.setRatingsCount(0);
        properties.setVersionRatingsCount(0);

        properties.setRating(Double.valueOf(0));
        properties.setVersionRating(Double.valueOf(0));

        ArtifactTag lastVersionTag = artifactTagService.findOneOrCreate(ArtifactTagEntity.LAST_VERSION);
        if (artifactEntry.getTagSet().contains(lastVersionTag))
        {
            properties.setIsLatestVersion(true);
        }
        else
        {
            properties.setIsLatestVersion(false);
        }

    }

    private PackageEntry createPackageEntry(String feedId,
                                            PathNupkg nupkg)
        throws NoSuchAlgorithmException,
        IOException,
        NugetFormatException
    {
        return new PackageEntry(nupkg){

            @Override
            protected String getRootUri()
            {
                return feedId;
            }
        };
    }

    @GetMapping(path = { "{storageId}/{repositoryId}/FindPackagesById()" }, produces = MediaType.APPLICATION_XML)
    public ResponseEntity<?> searchPackageById(@RepositoryMapping Repository repository,
                                               @RequestParam(name = "id", required = true) String packageId,
                                               HttpServletResponse response)
            throws JAXBException, IOException
    {
        final String storageId = repository.getStorage().getId();
        final String repositoryId = repository.getId();

        String normalisedPackageId = normaliseSearchTerm(packageId);

        NugetSearchRequest nugetSearchRequest = new NugetSearchRequest();
        nugetSearchRequest.setFilter(String.format("Id eq '%s'", packageId));
        repositorySearchEventListener.setNugetSearchRequest(nugetSearchRequest);

        RepositoryProvider provider = repositoryProviderRegistry.getProvider(repository.getType());

        Paginator paginator = new Paginator();
        paginator.setProperty("artifactCoordinates.coordinates.version");

        RepositorySearchRequest predicate = new RepositorySearchRequest(normalisedPackageId, Collections.singleton("nupkg"));

        Collection<? extends Nupkg> files = searchNupkg(storageId, repositoryId, provider, paginator, predicate);

        String feedId = getFeedUri(((ServletRequestAttributes) RequestContextHolder.currentRequestAttributes()).getRequest(),
                                   storageId,
                                   repositoryId);

        PackageFeed feed = transform(feedId, files);

        response.setHeader("Content-Type", MediaType.APPLICATION_XML);
        feed.writeXml(response.getOutputStream());

        return new ResponseEntity<>(HttpStatus.OK);
    }

    public Collection<? extends Nupkg> getPackages(Repository repository,
                                                   String filter,
                                                   String orderBy,
                                                   String searchTerm,
                                                   String targetFramework,
                                                   Long skip,
                                                   Integer top)
    {
        final String storageId = repository.getStorage().getId();
        final String repositoryId = repository.getId();

        RepositoryProvider provider = repositoryProviderRegistry.getProvider(repository.getType());

        Paginator paginator = new Paginator();
        paginator.setSkip(skip);
        paginator.setLimit(top);
        paginator.setProperty(orderBy);
        
        RepositorySearchRequest rootPredicate = createSearchPredicate(filter, searchTerm);

        return searchNupkg(storageId, repositoryId, provider, paginator, rootPredicate);
    }

    private List<PathNupkg> searchNupkg(String storageId,
                                        String repositoryId,
                                        RepositoryProvider provider,
                                        Paginator paginator,
                                        RepositorySearchRequest predicate)
    {
        return provider.search(storageId, repositoryId, predicate, paginator)
                       .stream()
                       .map(p -> {
                           try
                           {
                               return new PathNupkg((RepositoryPath) p);
                           }
                           catch (Exception e)
                           {
                               logger.error("Failed to resolve Nuget package path [{}]", p, e);
                               return null;
                           }
                       })
                       .collect(Collectors.toList());
    }

    private RepositorySearchRequest createSearchPredicate(String filter,
                                                          String searchTerm)
    {
        String artifactId = null;
        if (searchTerm != null && !searchTerm.trim().isEmpty())
        {
            artifactId = searchTerm;
        }
        else if (filter != null && !filter.trim().isEmpty())
        {
           NugetODataFilterQueryParser t = new NugetODataFilterQueryParser(filter);
           Predicate rootPredicate = t.parseQuery().getPredicate();
           List<Predicate> predicates = rootPredicate.getChildPredicateList();
           //TODO: rework NugetODataFilterQueryParser
           throw new UnsupportedOperationException();
        }
        return new RepositorySearchRequest(artifactId, Collections.singleton("nupkg"));
    }

    private String getFeedUri(HttpServletRequest request, String storageId, String repositoryId)
    {
        return String.format("%s://%s:%s%s/storages/%s/%s/",
                             request.getScheme(),
                             request.getServerName(),
                             request.getServerPort(),
                             request.getContextPath(),
                             storageId,
                             repositoryId);
    }

    @ApiOperation(value = "Used to get storage metadata")
    @ApiResponses(value = { @ApiResponse(code = HttpURLConnection.HTTP_OK, message = "The metadata was downloaded successfully."),
                            @ApiResponse(code = HttpURLConnection.HTTP_INTERNAL_ERROR, message = "An error occurred.") })
    @GetMapping(path = { "{storageId}/{repositoryId}/$metadata" }, produces = MediaType.APPLICATION_XML)
    public ResponseEntity<Resource> getMetadata()
    {
        InputStream inputStream = NugetArtifactController.class.getResourceAsStream("/metadata.xml");
        return new ResponseEntity<>(new InputStreamResource(inputStream), HttpStatus.OK);
    }

    /**
     * This method is used to check storage availability.<br>
     * For example NuGet pings the root without credentials to determine if the repository is healthy. If this receives
     * a 401 response then NuGet will prompt for authentication.
     * 
     * @return
     */
    @Override
    @ApiOperation(value = "Used to check storage availability")
    @ApiResponses(value = { @ApiResponse(code = HttpURLConnection.HTTP_OK, message = "Storage available."),
                            @ApiResponse(code = HttpURLConnection.HTTP_UNAUTHORIZED, message = "Storage requires authorization.") })
    @GetMapping(path = { "{storageId}/{repositoryId}", "checkRepositoryAccess" })
    @PreAuthorize("hasAuthority('ARTIFACTS_RESOLVE')")
    public ResponseEntity<String> checkRepositoryAccess()
    {
        return new ResponseEntity<>("success", HttpStatus.OK);
    }

    @ApiOperation(value = "Used to deploy a package")
    @ApiResponses(value = { @ApiResponse(code = HttpURLConnection.HTTP_OK, message = "The package was deployed successfully."),
                            @ApiResponse(code = HttpURLConnection.HTTP_INTERNAL_ERROR, message = "An error occurred.") })
    @PreAuthorize("hasAuthority('ARTIFACTS_DEPLOY')")
    @RequestMapping(path = "{storageId}/{repositoryId}/", method = RequestMethod.PUT, consumes = MediaType.MULTIPART_FORM_DATA)
    public ResponseEntity putPackage(@RequestHeader(name = "X-NuGet-ApiKey", required = false) String apiKey,
                                     @RepositoryMapping Repository repository,
                                     @RequestParam(value = "package") MultipartFile file,
                                     HttpServletRequest request)
    {
        final String storageId = repository.getStorage().getId();
        final String repositoryId = repository.getId();

        logger.info("Nuget push request: storageId-[{}]; repositoryId-[{}]", storageId, repositoryId);

        URI resourceUri;
        try
        {
            InputStream packagePartInputStream = file.getInputStream();
            resourceUri = storePackage(storageId, repositoryId, packagePartInputStream);
        }
        catch (Exception e)
        {
            logger.error("Failed to process Nuget push request: {}:{}", storageId, repositoryId, e);

            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).body(e.getMessage());
        }

        if (resourceUri == null)
        {
            // Return 501 status in case of empty package recieved.
            // For some reason nuget.exe sends empty package first.
            return ResponseEntity.status(HttpURLConnection.HTTP_BAD_REQUEST).build();
        }

        return ResponseEntity.created(resourceUri).build();
    }

    @ApiOperation(value = "Used to download a package")
    @ApiResponses(value = { @ApiResponse(code = HttpURLConnection.HTTP_OK, message = "The request was successfull."),
                            @ApiResponse(code = HttpURLConnection.HTTP_INTERNAL_ERROR, message = "Server error occurred."),
                            @ApiResponse(code = HttpURLConnection.HTTP_NOT_FOUND, message = "The requested path was not found."),
                            @ApiResponse(code = HttpURLConnection.HTTP_UNAVAILABLE, message = "Repository not in service currently.")})
    @PreAuthorize("hasAuthority('ARTIFACTS_RESOLVE')")
    @RequestMapping(path = "{storageId}/{repositoryId}/{commandName:(?:download|package)}/{packageId}/{packageVersion}",
                    method = {RequestMethod.GET, RequestMethod.HEAD},
                    produces = MediaType.APPLICATION_OCTET_STREAM)
    @AuditLog(value = AuditEventNameEnum.DOWNLOAD_EXCEPTION, target = "#repository.getStorage().getId() + '/' + #repository.getId() + '/' + #packageId + '/' + #packageVersion")
    public void downloadPackage(@RepositoryMapping Repository repository,
                                @ApiParam(value = "The packageId", required = true) @PathVariable(name = "packageId") String packageId,
                                @ApiParam(value = "The packageVersion", required = true) @PathVariable(name = "packageVersion") String packageVersion,
                                HttpServletResponse response,
                                HttpServletRequest request,
                                @RequestHeader HttpHeaders httpHeaders)
            throws Exception
    {
        getPackageInternal(repository, packageId, packageVersion, response, request, httpHeaders);
    }

    @ApiOperation(value = "Used to download a package")
    @ApiResponses(value = { @ApiResponse(code = HttpURLConnection.HTTP_OK, message = "The request was successfull."),
                            @ApiResponse(code = HttpURLConnection.HTTP_INTERNAL_ERROR, message = "Server error occurred."),
                            @ApiResponse(code = HttpURLConnection.HTTP_NOT_FOUND, message = "The requested path was not found."),
                            @ApiResponse(code = HttpURLConnection.HTTP_UNAVAILABLE, message = "Repository not in service currently.")})
    @PreAuthorize("hasAuthority('ARTIFACTS_RESOLVE')")
    @RequestMapping(path = "{storageId}/{repositoryId}/{packageId}/{packageVersion}",
                    method = {RequestMethod.GET, RequestMethod.HEAD},
                    produces = MediaType.APPLICATION_OCTET_STREAM)
    public void getPackage(@RepositoryMapping Repository repository,
                           @ApiParam(value = "The packageId", required = true) @PathVariable(name = "packageId") String packageId,
                           @ApiParam(value = "The packageVersion", required = true) @PathVariable(name = "packageVersion") String packageVersion,
                           HttpServletResponse response,
                           HttpServletRequest request,
                           @RequestHeader HttpHeaders httpHeaders)
            throws Exception
    {
        getPackageInternal(repository, packageId, packageVersion, response, request, httpHeaders);
    }

    private void getPackageInternal(Repository repository,
                                    String packageId,
                                    String packageVersion,
                                    HttpServletResponse response,
                                    HttpServletRequest request,
                                    HttpHeaders httpHeaders)
            throws Exception
    {
        final String storageId = repository.getStorage().getId();
        final String repositoryId = repository.getId();
        logger.info("Requested Nuget Package {},{}, {}, {}.", storageId, repositoryId, packageId, packageVersion);

        String fileName = String.format("%s.%s.nupkg", packageId, packageVersion);
        String path = String.format("%s/%s/%s", packageId, packageVersion, fileName);

        RepositoryPath repositoryPath = artifactResolutionService.resolvePath(storageId, repositoryId, path);
        vulnerabilityBlock(repositoryPath);
        if (provideArtifactDownloadResponse(request, response, httpHeaders, repositoryPath))
        {
            response.setHeader("Content-Disposition", String.format("attachment; filename=\"%s\"", fileName));
        }
    }


    private URI storePackage(String storageId,
                             String repositoryId,
                             InputStream is)
        throws Exception
    {
        try (TempNupkgFile nupkgFile = new TempNupkgFile(is))
        {
            Nuspec nuspec = nupkgFile.getNuspec();
            if (nuspec == null)
            {
                return null;
            }

            String nuspecId = nuspec.getId();

            SemanticVersion nuspecVersion = nuspec.getVersion();
            String path = String.format("%s/%s/%s.%s.nupkg",
                                        nuspecId,
                                        nuspecVersion,
                                        nuspecId,
                                        nuspecVersion);

            RepositoryPath repositoryPath = repositoryPathResolver.resolve(storageId, repositoryId, path);
            artifactManagementService.validateAndStore(repositoryPath, nupkgFile.getStream());

            Path nuspecFile = Files.createTempFile(nuspec.getId(), "nuspec");
            try (OutputStream outputStream = new BufferedOutputStream(Files.newOutputStream(nuspecFile)))
            {
                nuspec.saveTo(outputStream);
            }
            path = String.format("%s/%s/%s.nuspec", nuspecId, nuspecVersion, nuspecId);
            repositoryPath = repositoryPathResolver.resolve(storageId, repositoryId, path);
            try (InputStream bis = new BufferedInputStream(Files.newInputStream(nuspecFile)))
            {
                artifactManagementService.validateAndStore(repositoryPath, bis);
            }

            Path hashFile = Files.createTempFile(String.format("%s.%s", nuspecId, nuspecVersion),
                                                "nupkg.sha512");

            try (OutputStream bos = new BufferedOutputStream(Files.newOutputStream(hashFile)))
            {
                Writer writer = new OutputStreamWriter(bos);
                writer.write(nupkgFile.getHash());
                writer.flush();
                bos.flush();
            }

            path = String.format("%s/%s/%s.%s.nupkg.sha512",
                                 nuspecId,
                                 nuspecVersion,
                                 nuspecId,
                                 nuspecVersion);
            repositoryPath = repositoryPathResolver.resolve(storageId, repositoryId, path);
            try (InputStream bis = new BufferedInputStream(Files.newInputStream(hashFile)))
            {
                artifactManagementService.validateAndStore(repositoryPath, bis);
            }
        }

        return new URI("");
    }

    private String normaliseSearchTerm(String sourceValue)
    {
        if (sourceValue == null)
        {
            return null;
        }

        return sourceValue.replaceAll("['\"]", "");
    }

    // API_V3
    @ApiOperation(value = "PackageBaseAddress/3.0.0-枚举包版本")
    @GetMapping(path = "{storageId}/{repositoryId}/{packageId}/index.json",
                produces = MediaType.APPLICATION_JSON)
    public ResponseEntity<Resource> indexPackageV3(@RepositoryMapping Repository repository,
                                                   @PathVariable(name = "packageId") String packageId)
    {
        return null;
    }

    @ApiOperation(value = "PackageBaseAddress/3.0.0-下载包内容")
    @GetMapping(path = "{storageId}/{repositoryId}/{packageId}/{version}/{packageId}.{version}.nupkg",
                produces = MediaType.APPLICATION_OCTET_STREAM)
    public ResponseEntity<Resource> downloadPackageV3(@RepositoryMapping Repository repository,
                                                      @PathVariable(name = "packageId") String packageId,
                                                      @PathVariable(name = "version") String packageVersion)
    {
        return null;
    }

    @ApiOperation(value = "PackageBaseAddress/3.0.0-下载包清单")
    @GetMapping(path = "{storageId}/{repositoryId}/{packageId}/{version}/{packageID}.nuspec",
                produces = MediaType.APPLICATION_XML)
    public ResponseEntity<Resource> downloadNuspecV3(@RepositoryMapping Repository repository,
                                                    @PathVariable(name = "packageId") String packageId,
                                                    @PathVariable(name = "version") String packageVersion)
    {
        return null;
    }


    @ApiOperation(value = "PackagePublish/2.0.0-重新列出包")
    @PostMapping(path = "{storageId}/{repositoryId}/{packageId}/{packageVersion}")
    public ResponseEntity<Resource> relistPackage(@RepositoryMapping Repository repository,
                                                 @PathVariable(name = "packageId") String packageId,
                                                 @PathVariable(name = "packageVersion") String packageVersion)
    {
        return null;
    }

    /*
    *  SearchQueryService/3.5.0-搜索包
    *  兼容 SearchQueryService, SearchQueryService/3.0.0-beta 和 SearchQueryService/3.0.0-rc
    * */
    @ApiOperation(value = "SearchQueryService/3.5.0-搜索包")
    @GetMapping(path = "{storageId}/{repositoryId}/search")
    public ResponseEntity<Resource> searchPackage(@RepositoryMapping Repository repository,
                                                  @RequestParam(name = "q") String query,
                                                  @RequestParam(name = "skip") Integer skip,
                                                  @RequestParam(name = "take") Integer take,
                                                  @RequestParam(name = "prerelease") Boolean prerelease,
                                                  @RequestParam(name = "semVerLevel") String semVerLevel,
                                                  @RequestParam(name = "packageType") String packageType)
    {
        return null;
    }



}
