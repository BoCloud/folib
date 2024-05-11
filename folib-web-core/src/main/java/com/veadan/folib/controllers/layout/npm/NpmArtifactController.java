package com.veadan.folib.controllers.layout.npm;

import com.alibaba.fastjson.JSON;
import com.alibaba.fastjson.JSONObject;
import com.fasterxml.jackson.core.JsonFactory;
import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.core.JsonToken;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.google.common.collect.Lists;
import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.veadan.folib.artifact.coordinates.NpmArtifactCoordinates;
import com.veadan.folib.components.artifact.ArtifactComponent;
import com.veadan.folib.config.NpmLayoutProviderConfig.NpmObjectMapper;
import com.veadan.folib.controllers.BaseArtifactController;
import com.veadan.folib.data.criteria.Paginator;
import com.veadan.folib.domain.Artifact;
import com.veadan.folib.domain.ArtifactEntity;
import com.veadan.folib.domain.ArtifactIdGroupEntity;
import com.veadan.folib.enums.NpmPacketSuffix;
import com.veadan.folib.enums.NpmSubLayout;
import com.veadan.folib.model.request.OhpmLoginReq;
import com.veadan.folib.model.response.OhpmLoginRes;
import com.veadan.folib.model.response.OhpmPublishRes;
import com.veadan.folib.npm.NpmSearchRequest;
import com.veadan.folib.npm.NpmViewRequest;
import com.veadan.folib.npm.metadata.PackageFeed;
import com.veadan.folib.npm.metadata.PackageVersion;
import com.veadan.folib.npm.metadata.SearchResults;
import com.veadan.folib.providers.ProviderImplementationException;
import com.veadan.folib.providers.io.RepositoryFiles;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.providers.layout.*;
import com.veadan.folib.providers.repository.RepositoryProvider;
import com.veadan.folib.providers.repository.RepositoryProviderRegistry;
import com.veadan.folib.providers.repository.RepositorySearchRequest;
import com.veadan.folib.repository.NpmRepositoryFeatures.SearchPackagesEventListener;
import com.veadan.folib.repository.NpmRepositoryFeatures.ViewPackageEventListener;
import com.veadan.folib.storage.repository.Repository;
import com.veadan.folib.storage.validation.artifact.ArtifactCoordinatesValidationException;
import com.veadan.folib.users.userdetails.SpringSecurityUser;
import com.veadan.folib.web.LayoutRequestMapping;
import com.veadan.folib.web.RepositoryMapping;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;
import org.apache.commons.codec.digest.DigestUtils;
import org.apache.commons.compress.archivers.tar.TarArchiveEntry;
import org.apache.commons.compress.archivers.tar.TarArchiveInputStream;
import org.apache.commons.compress.compressors.gzip.GzipCompressorInputStream;
import org.apache.commons.io.FilenameUtils;
import org.apache.commons.io.IOUtils;
import org.apache.commons.lang3.StringUtils;
import org.graalvm.compiler.replacements.StringUTF16Substitutions;
import org.javatuples.Pair;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.scheduling.concurrent.ThreadPoolTaskExecutor;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.security.authentication.InsufficientAuthenticationException;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.Authentication;
import org.springframework.util.Assert;
import org.springframework.web.bind.annotation.*;

import javax.inject.Inject;
import javax.servlet.ServletInputStream;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.*;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
import java.nio.file.StandardOpenOption;
import java.security.NoSuchAlgorithmException;
import java.text.SimpleDateFormat;
import java.util.*;
import java.util.stream.Collectors;


/**
 * This Controller used to handle npm requests.
 *
 * @author @author veadan
 */
@RestController
@LayoutRequestMapping(NpmArtifactCoordinates.LAYOUT_NAME)
@Api(description = "npm坐标控制器", tags = "npm坐标控制器")
public class NpmArtifactController
        extends BaseArtifactController {

    private static final String FIELD_NAME_LENGTH = "length";

    private static final String FIELD_NAME_ATTACHMENTS = "_attachments";

    private static final String FIELD_NAME_VERSION = "versions";

    @Inject
    @NpmObjectMapper
    private ObjectMapper npmJacksonMapper;

    @Inject
    private RepositoryProviderRegistry repositoryProviderRegistry;

    @Inject
    private NpmPackageSupplier npmPackageSupplier;

    @Inject
    private NpmSearchResultSupplier npmSearchResultSupplier;

    @Inject
    private ViewPackageEventListener viewPackageEventListener;

    @Inject
    private SearchPackagesEventListener searcPackagesEventListener;

    @Inject
    private NpmUnpublishService npmUnpublishService;

    @Inject
    private ArtifactComponent artifactComponent;

    @Inject
    protected ApplicationEventPublisher eventPublisher;

    @Inject
    private ThreadPoolTaskExecutor asyncFetchRemotePackageThreadPoolTaskExecutor;

    @GetMapping(path = "{storageId}/{repositoryId}/-/v1/search")
    @PreAuthorize("hasAuthority('ARTIFACTS_RESOLVE')")
    public void search(@RepositoryMapping Repository repository,
                       @RequestParam(name = "text") String text,
                       @RequestParam(name = "size", defaultValue = "20") Integer size,
                       HttpServletResponse response)
            throws IOException {
        final String storageId = repository.getStorage().getId();
        final String repositoryId = repository.getId();

        NpmSearchRequest npmSearchRequest = new NpmSearchRequest();
        npmSearchRequest.setText(text);
        npmSearchRequest.setSize(size);

        searcPackagesEventListener.setNpmSearchRequest(npmSearchRequest);

        RepositoryProvider provider = repositoryProviderRegistry.getProvider(repository.getType());

        RepositorySearchRequest predicate = new RepositorySearchRequest(text, Collections.singleton("tgz"));
        Paginator paginator = new Paginator();
        paginator.setLimit(20);
        List<Path> searchResult = provider.search(storageId, repositoryId, predicate, paginator);

        SearchResults searchResults = new SearchResults();
        searchResult.stream().map(npmSearchResultSupplier).forEach(p -> {
            searchResults.getObjects().add(p);
        });

        Long count = provider.count(storageId, repositoryId, predicate);
        searchResults.setTotal(count.intValue());

        //Wed Oct 31 2018 05:01:19 GMT+0000 (UTC)
        SimpleDateFormat format = new SimpleDateFormat(NpmSearchResultSupplier.SEARCH_DATE_FORMAT);
        searchResults.setTime(format.format(new Date()));

        response.setContentType(MediaType.APPLICATION_JSON_VALUE);
        response.getOutputStream().write(npmJacksonMapper.writeValueAsBytes(searchResults));
    }

    @GetMapping(path = "{storageId}/{repositoryId}/-/binary/{artifactPath:.+}")
    @PreAuthorize("hasAuthority('ARTIFACTS_RESOLVE')")
    public void viewBinaryFeedWithScope(@RepositoryMapping Repository repository,
                                        @PathVariable(name = "storageId") String storageId,
                                        @PathVariable(name = "repositoryId") String repositoryId,
                                        @PathVariable(name = "artifactPath") String artifactPath,
                                        HttpServletRequest request,
                                        HttpServletResponse response,
                                        @RequestHeader HttpHeaders httpHeaders)
            throws Exception {
        String extension = FilenameUtils.getExtension(artifactPath);
        if (StringUtils.isNotBlank(extension)) {
            String prefix = String.format("/storages/%s/%s", storageId, repositoryId);
            String packageId = request.getRequestURI().substring(prefix.length() + 1);
            RepositoryPath repositoryPath = artifactResolutionService.resolvePath(storageId, repositoryId, packageId);
            vulnerabilityBlock(repositoryPath);
            provideArtifactDownloadResponse(request, response, httpHeaders, repositoryPath);
            return;
        }
        long startTime = System.currentTimeMillis();
        response.setContentType(MediaType.APPLICATION_JSON_VALUE);
        String prefix = String.format("/storages/%s/%s", storageId, repositoryId);
        String packageId = request.getRequestURI().substring(prefix.length());
        String binaryFeed = artifactComponent.getNpmArtifactIdGroupBinaryCache(repository, packageId);
        if (Objects.isNull(binaryFeed)) {
            String msg = "{\"error\":\"[NOT_FOUND] %s not found\"}";
            response.setStatus(HttpStatus.NOT_FOUND.value());
            response.getOutputStream().write(String.format(msg, packageId).getBytes());
            return;
        }
        try (InputStream inputStream = new ByteArrayInputStream(binaryFeed.getBytes(StandardCharsets.UTF_8))) {
            copyToResponse(inputStream, response);
        }
        logger.debug("[{}] viewPackageFeedWithScope storageId [{}] repositoryId [{}] packageId [{}] task time [{}] ms", this.getClass().getSimpleName(), repository.getStorage().getId(), repository.getId(), packageId, System.currentTimeMillis() - startTime);
    }

    @GetMapping(path = "{storageId}/{repositoryId}/{packageScope:[^-].+}/{packageName:[^-].+}/{packageVersion}")
    @PreAuthorize("hasAuthority('ARTIFACTS_RESOLVE')")
    public void viewPackageWithScope(@RepositoryMapping Repository repository,
                                     @PathVariable(name = "packageScope") String packageScope,
                                     @PathVariable(name = "packageName") String packageName,
                                     @PathVariable(name = "packageVersion") String packageVersion,
                                     HttpServletResponse response)
            throws Exception {
        final String storageId = repository.getStorage().getId();
        final String repositoryId = repository.getId();

        String packageId = NpmArtifactCoordinates.calculatePackageId(packageScope, packageName);
        final String packageSuffix = NpmSubLayout.OHNPM.getValue().equals(repository.getSubLayout()) ? NpmPacketSuffix.HAR.getValue() : NpmPacketSuffix.TGZ.getValue();
        NpmArtifactCoordinates c = NpmArtifactCoordinates.of(packageId, packageVersion,packageSuffix);

        NpmViewRequest npmSearchRequest = new NpmViewRequest();
        npmSearchRequest.setPackageId(packageId);
        npmSearchRequest.setVersion(packageVersion);
        viewPackageEventListener.setNpmSearchRequest(npmSearchRequest);

        RepositoryPath repositoryPath = artifactResolutionService.resolvePath(storageId, repositoryId, c.buildPath());
        if (repositoryPath == null) {
            response.setStatus(HttpStatus.NOT_FOUND.value());

            return;
        }

        NpmPackageDesc packageDesc = npmPackageSupplier.apply(repositoryPath);
        PackageVersion npmPackage = packageDesc.getNpmPackage();

        response.setContentType(MediaType.APPLICATION_JSON_VALUE);
        response.getOutputStream().write(npmJacksonMapper.writeValueAsBytes(npmPackage));
    }

    @GetMapping(path = "{storageId}/{repositoryId}/{packageScope:[^-].+}/{packageName}")
    @PreAuthorize("hasAuthority('ARTIFACTS_RESOLVE')")
    public void viewPackageFeedWithScope(@RepositoryMapping Repository repository,
                                         @PathVariable(name = "packageScope") String packageScope,
                                         @PathVariable(name = "packageName") String packageName,
                                         HttpServletRequest request,
                                         HttpServletResponse response,
                                         @RequestHeader HttpHeaders httpHeaders)
            throws Exception {
        long startTime = System.currentTimeMillis();
        response.setContentType(MediaType.APPLICATION_JSON_VALUE);
        String subLayout = repository.getSubLayout();
        RepositorySearchRequest predicate = createSearchPredicate(packageScope, packageName, subLayout);
        String packageId = NpmArtifactCoordinates.calculatePackageId(packageScope, packageName);
        List<String> coordinateValues = NpmSubLayout.OHNPM.getValue().equals(subLayout) ? Lists.newArrayList("har") : Lists.newArrayList("tgz");
        PackageFeed packageFeed = artifactComponent.getNpmArtifactIdGroupCache(repository, predicate.getArtifactId(), coordinateValues, predicate);
        if (Objects.isNull(packageFeed)) {
            String msg = "{\"error\":\"[NOT_FOUND] %s not found\"}";
            response.setStatus(HttpStatus.NOT_FOUND.value());
            response.getOutputStream().write(String.format(msg, packageId).getBytes());
            return;
        }
        JSONObject jsonobj =  JSON.parseObject(json2);
        try (InputStream inputStream = new ByteArrayInputStream(npmJacksonMapper.writeValueAsBytes(jsonobj))) {
            copyToResponse(inputStream, response);
        }
        logger.debug("[{}] viewPackageFeedWithScope storageId [{}] repositoryId [{}] packageId [{}] task time [{}] ms", this.getClass().getSimpleName(), repository.getStorage().getId(), repository.getId(), packageId, System.currentTimeMillis() - startTime);
    }

    private String generateRevisionHashcode(PackageFeed packageFeed) {
        String versionsShasum = packageFeed.getVersions().getAdditionalProperties()
                .values()
                .stream()
                .map(x -> x.getDist().getShasum())
                .collect(Collectors.joining());
        return packageFeed.getVersions().getAdditionalProperties().size() + "-" +
                DigestUtils.sha1Hex(versionsShasum).substring(0, 16);
    }

    @GetMapping(path = "{storageId}/{repositoryId}/{packageName}")
    @PreAuthorize("hasAuthority('ARTIFACTS_RESOLVE')")
    public void viewPackageFeed(@RepositoryMapping Repository repository,
                                @PathVariable(name = "packageName") String packageName,
                                HttpServletRequest request,
                                HttpServletResponse response,
                                @RequestHeader HttpHeaders httpHeaders)
            throws Exception {
        viewPackageFeedWithScope(repository, null, packageName, request, response, httpHeaders);
    }

    private RepositorySearchRequest createSearchPredicate(String packageScope,
                                                          String packageName,String subLayout) {
        List<String> coordinateValues = NpmSubLayout.OHNPM.getValue().equals(subLayout) ? Lists.newArrayList("har") : Lists.newArrayList("tgz");
        RepositorySearchRequest rootPredicate = new RepositorySearchRequest(
                NpmArtifactCoordinates.calculatePackageId(packageScope, packageName), Lists.newArrayList(coordinateValues));

        return rootPredicate;
    }

    @PreAuthorize("hasAuthority('ARTIFACTS_RESOLVE')")
    @RequestMapping(path = "{storageId}/{repositoryId}/{packageScope}/{packageName}/-/{packageNameWithVersion}.{packageExtension}",
            method = {RequestMethod.GET,
                    RequestMethod.HEAD})
    public ResponseEntity<Object> downloadPackageWithScope(@RepositoryMapping Repository repository,
                                                           @PathVariable(name = "packageScope") String packageScope,
                                                           @PathVariable(name = "packageName") String packageName,
                                                           @PathVariable(name = "packageNameWithVersion") String packageNameWithVersion,
                                                           @PathVariable(name = "packageExtension") String packageExtension,
                                                           @RequestHeader HttpHeaders httpHeaders,
                                                           HttpServletRequest request,
                                                           HttpServletResponse response)
            throws Exception {
        long startTime = System.currentTimeMillis();
        final String storageId = repository.getStorage().getId();
        final String repositoryId = repository.getId();
        String packageVersion = "";
        //Example of packageNameWithVersion  core-9.0.1-next.8.tgz
        boolean isPackage =   packageNameWithVersion.startsWith("package-") && packageExtension.endsWith("json");
        String artifactPath = "";
        if (!isPackage) {
            if (!packageNameWithVersion.startsWith(packageName + "-")) {
                response.setStatus(HttpServletResponse.SC_BAD_REQUEST);
                return null;
            }
            packageVersion = getPackageVersion(packageNameWithVersion, packageName);
            NpmArtifactCoordinates coordinates;
            try {
                final String packageSuffix = NpmSubLayout.OHNPM.getValue().equals(repository.getSubLayout()) ? NpmPacketSuffix.HAR.getValue() : NpmPacketSuffix.TGZ.getValue();
                coordinates = NpmArtifactCoordinates.of(String.format("%s/%s", packageScope, packageName), packageVersion,packageSuffix);
                artifactPath = coordinates.buildPath();
            } catch (IllegalArgumentException e) {
                response.setStatus(HttpStatus.BAD_REQUEST.value());
                response.getWriter().write(e.getMessage());
                return null;
            }
            RepositoryPath repositoryPath = artifactResolutionService.resolvePath(storageId, repositoryId, artifactPath);
            vulnerabilityBlock(repositoryPath);
            provideArtifactDownloadResponse(request, response, httpHeaders, repositoryPath);
            logger.debug("[{}] downloadPackageWithScope [{}] task time [{}] ms", this.getClass().getSimpleName(), repositoryPath.toString(), System.currentTimeMillis() - startTime);
        } else {
            packageVersion = getPackageJsonVersion(packageNameWithVersion);
            artifactPath = String.format("%s/%s/%s/%s", packageScope, packageName, packageVersion, NpmLayoutProvider.PACKAGE_JSON);
            RepositoryPath repositoryPath = artifactResolutionService.resolvePath(storageId, repositoryId, artifactPath);
            vulnerabilityBlock(repositoryPath);
            String packages = artifactComponent.readRepositoryPathContent(repositoryPath);
            GsonBuilder gsonBuilder = new GsonBuilder();
            gsonBuilder.setPrettyPrinting();
            Gson gson = gsonBuilder.create();
            return ResponseEntity.ok(gson.toJson(packages));
        }
        return null;
    }

    @PreAuthorize("hasAuthority('ARTIFACTS_RESOLVE')")
    @RequestMapping(path = "{storageId}/{repositoryId}/{packageName}/-/{packageNameWithVersion}.{packageExtension}",
            method = {RequestMethod.GET,
                    RequestMethod.HEAD})
    public ResponseEntity<Object> downloadPackage(@RepositoryMapping Repository repository,
                                                  @PathVariable(name = "packageName") String packageName,
                                                  @PathVariable(name = "packageNameWithVersion") String packageNameWithVersion,
                                                  @PathVariable(name = "packageExtension") String packageExtension,
                                                  @RequestHeader HttpHeaders httpHeaders,
                                                  HttpServletRequest request,
                                                  HttpServletResponse response)
            throws Exception {
        long startTime = System.currentTimeMillis();
        final String storageId = repository.getStorage().getId();
        final String repositoryId = repository.getId();
        String packageVersion = "";
        //Example of packageNameWithVersion core-9.0.1-next.8.tgz
        boolean isPackage = packageNameWithVersion.startsWith("package-") && packageExtension.endsWith("json");
        String artifactPath = "";
        if (!isPackage) {
            if (!packageNameWithVersion.startsWith(packageName + "-")) {
                response.setStatus(HttpServletResponse.SC_BAD_REQUEST);
                return null;
            }
            packageVersion = getPackageVersion(packageNameWithVersion, packageName);

            NpmArtifactCoordinates coordinates;
            try {
                final String packageSuffix = NpmSubLayout.OHNPM.getValue().equals(repository.getSubLayout()) ? NpmPacketSuffix.HAR.getValue() : NpmPacketSuffix.TGZ.getValue();
                coordinates = NpmArtifactCoordinates.of(packageName, packageVersion,packageSuffix);
            } catch (IllegalArgumentException e) {
                response.setStatus(HttpStatus.BAD_REQUEST.value());
                response.getWriter().write(e.getMessage());
                return null;
            }

            RepositoryPath path = artifactResolutionService.resolvePath(storageId, repositoryId, coordinates.buildPath());
            vulnerabilityBlock(path);
            provideArtifactDownloadResponse(request, response, httpHeaders, path);
            logger.debug("[{}] downloadPackage [{}] task time [{}] ms", this.getClass().getSimpleName(), path.toString(), System.currentTimeMillis() - startTime);
        } else {
            packageVersion = getPackageJsonVersion(packageNameWithVersion);
            artifactPath = String.format("%s/%s/%s/%s", packageName, packageName, packageVersion, NpmLayoutProvider.PACKAGE_JSON);
            RepositoryPath repositoryPath = artifactResolutionService.resolvePath(storageId, repositoryId, artifactPath);
            vulnerabilityBlock(repositoryPath);
            String packages = artifactComponent.readRepositoryPathContent(repositoryPath);
            GsonBuilder gsonBuilder = new GsonBuilder();
            gsonBuilder.setPrettyPrinting();
            Gson gson = gsonBuilder.create();
            return ResponseEntity.ok(gson.toJson(packages));
        }
        return null;
    }

    //@PreAuthorize("hasAuthority('ARTIFACTS_DEPLOY')")
    @PutMapping(path = "{storageId}/{repositoryId}/{name:.+}", consumes = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity publish(@RepositoryMapping Repository repository,
                                  @PathVariable(name = "name") String name,
                                  HttpServletRequest request)
            throws Exception {
        if (nameContainsRevision(name)) {
            return ResponseEntity.status(HttpStatus.OK).build();
        }
        final String storageId = repository.getStorage().getId();
        final String repositoryId = repository.getId();
        final String subLayout = repository.getSubLayout();

        logger.info("npm publish request for {}/{}/{}", storageId, repositoryId, name);
        Pair<PackageVersion, Path> packageEntry;
        try {
            packageEntry = extractPackage(name, request.getInputStream(),subLayout);
        } catch (IllegalArgumentException e) {
            logger.error("Failed to extract npm package data", e);
            return ResponseEntity.badRequest().build();
        }

        final String packageSuffix = NpmSubLayout.OHNPM.getValue().equals(subLayout) ? NpmPacketSuffix.HAR.getValue() : NpmPacketSuffix.TGZ.getValue();
        PackageVersion packageJson = packageEntry.getValue0();
        Path packageTgz = packageEntry.getValue1();
        NpmArtifactCoordinates coordinates = NpmArtifactCoordinates.of(name, packageJson.getVersion(),packageSuffix);
        storeNpmPackage(repository, coordinates, packageJson, packageTgz,repository.getSubLayout());
        artifactComponent.updateArtifactIdGroup(new ArtifactIdGroupEntity(storageId, repositoryId, coordinates.getId()), "");
        if(NpmSubLayout.OHNPM.getValue().equals(repository.getSubLayout())){
            OhpmPublishRes res =  OhpmPublishRes.builder()
                    .additionalMsg("")
                    .success(true)
                    .build();
            return ResponseEntity.ok(res);
        }
        return ResponseEntity.ok("");
    }

    /**
     * Resolves if a passed name contains '/-rev' substring.
     * Npm 'unpublish' a single version package comprises 4 requests: GET, PUT, GET, DELETE, and
     * PUT method has a path that Folib maps on
     * {@link NpmArtifactController#publish(Repository, String, HttpServletRequest)}.
     * Example of PUT path: http://localhost:8080/@scope/package/-rev/0-0000000000000000.
     * As publishing doesn't play any role in 'unpublish' process, it should be skipped.
     *
     * @param name name from path "{storageId}/{repositoryId}/{name:.+}"
     * @return true if contains, false if not. If true PUT stage of 'unpublish' will be skipped.
     */
    private boolean nameContainsRevision(String name) {
        if (name.contains("/-rev/")) {
            logger.warn("Url comprises '/-rev/' sub path");

            return true;
        }
        return false;
    }

    @PreAuthorize("hasAuthority('ARTIFACTS_VIEW')")
    @PutMapping(path = "{storageId}/{repositoryId}/-/user/org.couchdb.user:{username}",
            consumes = MediaType.APPLICATION_JSON_VALUE,
            produces = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity addUser(Authentication authentication) {
        if (authentication == null || !authentication.isAuthenticated()) {
            throw new InsufficientAuthenticationException("unauthorized");
        }

        if (!(authentication instanceof UsernamePasswordAuthenticationToken)) {
            return toResponseEntityError("Unsupported authentication class " + authentication.getClass().getName());
        }

        Object principal = authentication.getPrincipal();
        if (!(principal instanceof SpringSecurityUser)) {
            return toResponseEntityError(
                    "Unsupported authentication principal " + Optional.ofNullable(principal).orElse(null));
        }

        return ResponseEntity
                .status(HttpStatus.CREATED)
                .body("{\"ok\":\"user '" + authentication.getName() + "' created\"}");
    }

    @DeleteMapping(path = "{storageId}/{repositoryId}/{packageScope}/{packageName}/-rev/{rev}")
    @PreAuthorize("hasAuthority('ARTIFACTS_DELETE')")
    public ResponseEntity unpublishPackageWithScope(@RepositoryMapping Repository repository,
                                                    @PathVariable(name = "packageScope") String packageScope,
                                                    @PathVariable(name = "packageName") String packageName,
                                                    @PathVariable(name = "rev") String rev) {

        logger.info("Npm unpublish a package request: storageId-[{}]; repositoryId-[{}]; packageName-[{}]; revision-[{}];",
                repository.getStorage().getId(),
                repository.getId(),
                packageName,
                rev);

        NpmUnpublishService.Result result = npmUnpublishService.unpublishPackage(repository, packageScope, packageName);

        return processUnpublishResult(result);
    }

    /**
     * Unpublish a single version of a specified package. This mapping works for npm versions > 6.5.0.
     *
     * @param repository   repository
     * @param packageScope package scope
     * @param packageName  package name
     * @param tarball      tarball
     * @param rev          revision value
     * @return result via {@link ResponseEntity} with HTTP status
     * @throws Exception
     */
    @DeleteMapping(path = "{storageId}/{repositoryId}/{r1}/{r2}/{r3}/{packageScope}/{packageName}/-/{tarball}/-rev/{rev}")
    @PreAuthorize("hasAuthority('ARTIFACTS_DELETE')")
    public ResponseEntity unpublishVersionWithScope(@RepositoryMapping Repository repository,
                                                    @PathVariable(name = "packageScope") String packageScope,
                                                    @PathVariable(name = "packageName") String packageName,
                                                    @PathVariable(name = "tarball") String tarball,
                                                    @PathVariable(name = "rev") String rev) {

        final String version = getPackageVersion(tarball, packageName).replace(".tgz", "");

        logger.info("Npm unpublish a single version request: storageId-[{}]; repositoryId-[{}]; packageName-[{}]; tarball-[{}]; revision-[{}];",
                repository.getStorage().getId(),
                repository.getId(),
                packageName,
                tarball,
                rev);

        NpmUnpublishService.Result result = npmUnpublishService.unpublishSingleVersion(repository,
                packageScope,
                packageName,
                tarball,
                version);
        return processUnpublishResult(result);
    }

    /**
     * Unpublish a single version of a specified package. This mapping works for npm versions <= 6.5.0.
     *
     * @param repository   repository
     * @param packageScope package scope
     * @param packageName  package name
     * @param tarball      tarball
     * @param rev          revision value
     * @return result via {@link ResponseEntity} with HTTP status
     * @throws Exception
     */
    @DeleteMapping(path = "{storageId}/{repositoryId}/{packageScope}/{packageName}/-/{tarball}/-rev/{rev}")
    @PreAuthorize("hasAuthority('ARTIFACTS_DELETE')")
    public ResponseEntity unpublishVersionWithScopeV5(@RepositoryMapping Repository repository,
                                                      @PathVariable(name = "packageScope") String packageScope,
                                                      @PathVariable(name = "packageName") String packageName,
                                                      @PathVariable(name = "tarball") String tarball,
                                                      @PathVariable(name = "rev") String rev) {

        final String version = getPackageVersion(tarball, packageName).replace(".tgz", "");

        logger.info("Npm unpublish a single version request: storageId-[{}]; repositoryId-[{}]; packageName-[{}]; tarball-[{}]; revision-[{}];",
                repository.getStorage().getId(),
                repository.getId(),
                packageName,
                tarball,
                rev);

        NpmUnpublishService.Result result = npmUnpublishService.unpublishSingleVersion(repository,
                packageScope,
                packageName,
                tarball,
                version);
        return processUnpublishResult(result);
    }

    @DeleteMapping(path = "{storageId}/{repositoryId}/{packageName}/-rev/{rev}")
    @PreAuthorize("hasAuthority('ARTIFACTS_DELETE')")
    public ResponseEntity unpublishPackage(@RepositoryMapping Repository repository,
                                           @PathVariable(name = "packageName") String packageName,
                                           @PathVariable(name = "rev") String rev) {
        return unpublishPackageWithScope(repository, null, packageName, rev);
    }

    @DeleteMapping(path = "{storageId}/{repositoryId}/{r1}/{r2}/{r3}/{packageName}/-/{tarball}/-rev/{rev}")
    @PreAuthorize("hasAuthority('ARTIFACTS_DELETE')")
    public ResponseEntity unpublishVersion(@RepositoryMapping Repository repository,
                                           @PathVariable(name = "packageName") String packageName,
                                           @PathVariable(name = "tarball") String tarball,
                                           @PathVariable(name = "rev") String rev) {
        return unpublishVersionWithScope(repository, null, packageName, tarball, rev);
    }

    @DeleteMapping(path = "{storageId}/{repositoryId}/{packageName}/-/{tarball}/-rev/{rev}")
    @PreAuthorize("hasAuthority('ARTIFACTS_DELETE')")
    public ResponseEntity unpublishVersionV5(@RepositoryMapping Repository repository,
                                             @PathVariable(name = "packageName") String packageName,
                                             @PathVariable(name = "tarball") String tarball,
                                             @PathVariable(name = "rev") String rev) {
        return unpublishVersionWithScopeV5(repository, null, packageName, tarball, rev);
    }

    @ApiOperation(value = "ohpm登录")
    @ApiResponses(value = {
            @ApiResponse(code = 200, message = "Successful operation", response = OhpmLoginRes.class),
            @ApiResponse(code = 401, message = "Unauthorized"),
            @ApiResponse(code = 403, message = "Forbidden"),
    })
    @PostMapping(path = "{storageId}/{repositoryId}/login")
    public ResponseEntity<?> ohpmLogin(@PathVariable(name = "storageId") String storageId,
                                       @PathVariable(name = "repositoryId") String repositoryId,
                                       @RequestBody OhpmLoginReq ohpmLoginReq) {
        if (ohpmLoginReq.getPublishId() != null) {
            OhpmLoginRes ohpmLoginRes =OhpmLoginRes .builder()
                    .success(true)
                    .token(ohpmLoginReq.getPublishId())
                    .message("")
                    .build();
            return ResponseEntity.ok(ohpmLoginRes);
        }
        return ResponseEntity.status(HttpStatus.UNAUTHORIZED).build();
    }
    private void storeNpmPackage(Repository repository,
                                 NpmArtifactCoordinates coordinates,
                                 PackageVersion packageDef,
                                 Path packageTgzTmp,String npmSubLayout)
            throws IOException,
            ProviderImplementationException,
            NoSuchAlgorithmException,
            ArtifactCoordinatesValidationException {
        RepositoryPath repositoryPath = repositoryPathResolver.resolve(repository, coordinates);
        try (InputStream is = new BufferedInputStream(Files.newInputStream(packageTgzTmp))) {
            artifactManagementService.validateAndStore(repositoryPath, is);
        }
        Path packageJsonTmp = extractPackageJson(packageTgzTmp, npmSubLayout);

        String packageName = NpmSubLayout.OHNPM.getValue().equals(npmSubLayout) ? "oh-package.json5" : "package.json";
        RepositoryPath packageJsonPath = repositoryPathResolver.resolve(repository,
                repositoryPath.resolveSibling(packageName));
        try (InputStream is = new BufferedInputStream(Files.newInputStream(packageJsonTmp))) {
            artifactManagementService.validateAndStore(packageJsonPath, is);
        }



        String shasum = Optional.ofNullable(packageDef.getDist()).map(p -> p.getShasum()).orElse(null);
        if (shasum == null) {
            logger.warn("No checksum provided for package [{}]", packageDef.getName());
            return;
        }

        String packageFileName = repositoryPath.getFileName().toString();
        RepositoryPath checksumPath = repositoryPath.resolveSibling(packageFileName + ".sha1");
        artifactManagementService.validateAndStore(checksumPath,
                new ByteArrayInputStream(shasum.getBytes(StandardCharsets.UTF_8)));

        Files.delete(packageTgzTmp);
        Files.delete(packageJsonTmp);

    }

    private Pair<PackageVersion, Path> extractPackage(String packageName,
                                                      ServletInputStream in,String subLayout)
            throws IOException {
        Path packageSourceTmp = Files.createTempFile("package", "source");
        Files.copy(in, packageSourceTmp, StandardCopyOption.REPLACE_EXISTING);

        PackageVersion packageVersion = null;
        Path packageTgzPath = null;

        JsonFactory jfactory = new JsonFactory();
        try (InputStream tmpIn = new BufferedInputStream(Files.newInputStream(packageSourceTmp));
             JsonParser jp = jfactory.createParser(tmpIn);) {
            jp.setCodec(npmJacksonMapper);

            Assert.isTrue(jp.nextToken() == JsonToken.START_OBJECT, "npm package source should be JSON object.");

            while (jp.nextToken() != null) {
                String fieldName = jp.getCurrentName();
                // read value
                if (fieldName == null) {
                    continue;
                }
                switch (fieldName) {
                    case FIELD_NAME_VERSION:
                        jp.nextValue();
                        JsonNode node = jp.readValueAsTree();
                        Assert.isTrue(node.size() == 1, "npm package source should contain only one version.");

                        JsonNode packageJsonNode = node.iterator().next();
                        packageVersion = extractPackageVersion(packageName, packageJsonNode.toString());

                        break;
                    case FIELD_NAME_ATTACHMENTS:
                        Assert.isTrue(jp.nextToken() == JsonToken.START_OBJECT,
                                String.format(
                                        "Failed to parse npm package source for illegal type [%s] of attachment.",
                                        jp.currentToken().name()));

                        String packageAttachmentName = jp.nextFieldName();
                        logger.info(String.format("Found npm package attachment [%s]", packageAttachmentName));

                        moveToAttachment(jp, packageAttachmentName);
                        packageTgzPath = extractPackage(jp, subLayout);

                        jp.nextToken();
                        jp.nextToken();

                        break;
                }
            }
        }

        Files.delete(packageSourceTmp);

        if (packageVersion == null || packageTgzPath == null) {
            throw new IllegalArgumentException(
                    String.format("Failed to parse npm package source for [%s], attachment not found", packageName));
        }

        return Pair.with(packageVersion, packageTgzPath);
    }

    private Path extractPackage(JsonParser jp,String subLayout)
            throws IOException {
        final String suffix = NpmSubLayout.NPM.getValue().equals(subLayout) ? NpmPacketSuffix.TGZ.getValue() : NpmPacketSuffix.HAR.getValue();
        Path packageTgzTmp = Files.createTempFile("package", suffix);
        try (OutputStream packageTgzOut = new BufferedOutputStream(Files.newOutputStream(packageTgzTmp,
                StandardOpenOption.TRUNCATE_EXISTING))) {
            jp.readBinaryValue(packageTgzOut);
        }

        long packageSize = Files.size(packageTgzTmp);

        Assert.isTrue(FIELD_NAME_LENGTH.equals(jp.nextFieldName()), "Failed to validate package content length.");
        jp.nextToken();

        Assert.isTrue(packageSize == jp.getLongValue(), "Invalid package content length.");
        jp.nextToken();

        return packageTgzTmp;
    }

    private Path extractPackageJson(Path packageTgzTmp,String npmSubLayout)
            throws IOException {
        String packageJsonSource;
        try (InputStream packageTgzIn = new BufferedInputStream(Files.newInputStream(packageTgzTmp))) {
            packageJsonSource = extrectPackageJson(packageTgzIn,npmSubLayout);
        }
        String packageName = NpmSubLayout.OHNPM.getValue().equals(npmSubLayout) ? "oh-package.json5" : "package";
        String suffix = NpmSubLayout.OHNPM.getValue().equals(npmSubLayout) ? "json5" : "json";
        Path packageJsonTmp = Files.createTempFile(packageName, suffix);
        assert packageJsonSource != null;
        Files.write(packageJsonTmp, packageJsonSource.getBytes(StandardCharsets.UTF_8),
                StandardOpenOption.TRUNCATE_EXISTING);

        return packageJsonTmp;
    }

    private void moveToAttachment(JsonParser jp,
                                  String packageAttachmentName)
            throws IOException {
        Assert.isTrue(jp.nextToken() == JsonToken.START_OBJECT,
                String.format(
                        "Failed to parse npm package source for [%s], illegal attachment content type [%s].",
                        packageAttachmentName, jp.currentToken().name()));

        jp.nextToken();
        String contentType = jp.nextTextValue();
        Assert.isTrue(MediaType.APPLICATION_OCTET_STREAM_VALUE.equals(contentType),
                String.format("Failed to parse npm package source for [%s], unknown content type [%s]",
                        packageAttachmentName, contentType));

        String dataFieldName = jp.nextFieldName();
        Assert.isTrue("data".equals(dataFieldName),
                String.format("Failed to parse npm package source for [%s], data not found",
                        packageAttachmentName));

        jp.nextToken();
    }

    private PackageVersion extractPackageVersion(String packageName,
                                                 String packageJsonSource)
            throws IOException {
        PackageVersion packageVersion;
        try {
            packageVersion = npmJacksonMapper.readValue(packageJsonSource, PackageVersion.class);
        } catch (JsonProcessingException e) {
            throw new IllegalArgumentException(String.format("Failed to parse package.json info for [%s]", packageName),
                    e);
        }
        Assert.isTrue(packageName.equals(packageVersion.getName()),
                String.format("Package name [%s] don't match with [%s].", packageVersion.getName(), packageName));

        return packageVersion;
    }

    private String extrectPackageJson(InputStream in,String subLayout)
            throws IOException {
        GzipCompressorInputStream gzipIn = new GzipCompressorInputStream(in);
        try (TarArchiveInputStream tarIn = new TarArchiveInputStream(gzipIn)) {
            TarArchiveEntry entry;

            while ((entry = (TarArchiveEntry) tarIn.getNextEntry()) != null) {

                String packageJsonPath = NpmSubLayout.OHNPM.getValue().equals(subLayout) ?
                        NpmLayoutProvider.OHPM_PACKAGE_JSON_PATH :
                        NpmLayoutProvider.DEFAULT_PACKAGE_JSON_PATH;
                if (!entry.getName().equals(packageJsonPath)) {
                    continue;
                }

                StringWriter writer = new StringWriter();
                IOUtils.copy(tarIn, writer, StandardCharsets.UTF_8);
                return writer.toString();
            }

            return null;
        }
    }

    private String getPackageVersion(String packageNameWithVersion,
                                     String packageName) {
        return packageNameWithVersion.substring(packageName.length() + 1);
    }

    private String getPackageJsonVersion(String packageJsonNameWithVersion) {
        return packageJsonNameWithVersion.substring("package".length() + 1);
    }

    private ResponseEntity processUnpublishResult(NpmUnpublishService.Result result) {
        switch (result) {
            case INTERNAL_SERVER_ERROR:
                return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).build();
            case ARTIFACT_DOES_NOT_EXIST:
            case UNPUBLISHED:
                return ResponseEntity.status(HttpStatus.OK).build();
            case UNPUBLISH_DISABLED:
                ResponseEntity.status(HttpStatus.BAD_REQUEST)
                        .body("Enable 'unpublish' at first");
            default:
                return ResponseEntity.status(HttpStatus.BAD_REQUEST).build();
        }
    }

    protected Artifact provideArtifact(RepositoryPath repositoryPath) throws IOException {
        return Optional.ofNullable(repositoryPath.getArtifactEntry())
                .orElse(new ArtifactEntity(repositoryPath.getStorageId(), repositoryPath.getRepositoryId(),
                        RepositoryFiles.readCoordinates(repositoryPath)));
    }

    String json ="{\n" +
            "    \"name\": \"@ohos/lottie\",\n" +
            "    \"dist-tags\": {\n" +
            "        \"latest\": \"2.0.10\"\n" +
            "    },\n" +
            "    \"versions\": {\n" +
            "        \"2.0.10\": {\n" +
            "            \"types\": \"index.d.ts\",\n" +
            "            \"keywords\": [\n" +
            "                \"OpenHarmony\",\n" +
            "                \"HarmonyOS\",\n" +
            "                \"Lottie\"\n" +
            "            ],\n" +
            "            \"author\": {\n" +
            "                \"name\": \"ohos_tpc\",\n" +
            "                \"url\": \"\",\n" +
            "                \"email\": \"\"\n" +
            "            },\n" +
            "            \"ohos\": {\n" +
            "                \"org\": \"opensource\"\n" +
            "            },\n" +
            "            \"description\": \"lottie是一个适用于OpenHarmony的动画库，它可以使用Bodymovin解析以json格式导出的Adobe After Effects动画，并在移动设备上进行本地渲染\",\n" +
            "            \"_ohpmVersion\": \"1.2.4\",\n" +
            "            \"dist\": {\n" +
            "                \"integrity\": \"sha512-HBWibLErld6QJaQonrygd2uhCBWs98QzZQzeMxZUcBiha8+2YVRDwh5lwCzy0ZuedVenT61EhDqZdOr8MqPFHg==\",\n" +
            "                \"shasum\": \"2e96f125a63dce402b8a6636b68130ed409b06c7\",\n" +
            "                \"tarball\": \"http://localhost:38080/storages/public-project/ohpm-local/@ohos/lottie/-/lottie-2.0.10.har\"\n" +
            "            },\n" +
            "            \"main\": \"src/main/js/modules/full.js\",\n" +
            "            \"repository\": \"https://gitee.com/openharmony-tpc/lottie.git\",\n" +
            "            \"type\": \"module\",\n" +
            "            \"version\": \"2.0.10\",\n" +
            "            \"tags\": [\n" +
            "                \"Animation\"\n" +
            "            ],\n" +
            "            \"dependencies\": {},\n" +
            "            \"license\": \"MIT\",\n" +
            "            \"devDependencies\": {},\n" +
            "            \"name\": \"@ohos/lottie\",\n" +
            "            \"_id\": \"@ohos/lottie@2.0.10\",\n" +
            "            \"_nodeVersion\": \"16.16.0\"\n" +
            "        }\n" +
            "    },\n" +
            "    \"maintainers\": [],\n" +
            "    \"time\": {\n" +
            "        \"modified\": \"2024-05-10T16:43:52.571Z\",\n" +
            "        \"created\": \"2024-05-10T16:43:52.571Z\",\n" +
            "        \"2.0.10\": \"2024-05-10T16:43:52.571Z\"\n" +
            "    },\n" +
            "    \"keywords\": [],\n" +
            "    \"_rev\": \"1-2979567f9f807f26\",\n" +
            "    \"description\": \"lottie是一个适用于OpenHarmony的动画库，它可以使用Bodymovin解析以json格式导出的Adobe After Effects动画，并在移动设备上进行本地渲染\",\n" +
            "    \"_id\": \"@ohos/lottie\",\n" +
            "    \"_id\": \"@ohos/lottie\"\n" +
            "}";

    String json2 = "{\n" +
            "    \"name\": \"@ohos/lottie\",\n" +
            "    \"dist-tags\": {\n" +
            "        \"latest\": \"2.0.10\"\n" +
            "    },\n" +
            "    \"versions\": {\n" +
            "        \"2.0.10\": {\n" +
            "            \"name\": \"@ohos/lottie\",\n" +
            "            \"version\": \"2.0.10\",\n" +
            "            \"keywords\": [],\n" +
            "            \"licenses\": [],\n" +
            "            \"contributors\": [],\n" +
            "            \"maintainers\": [],\n" +
            "            \"files\": [],\n" +
            "            \"man\": [],\n" +
            "            \"bundledDependencies\": [],\n" +
            "            \"os\": [],\n" +
            "            \"_ohpmVersion\": \"1.2.4\",\n" +
            "            \"cpu\": [],\n" +
            "            \"dist\": {\n" +
            "                \"integrity\": \"sha512-HBWibLErld6QJaQonrygd2uhCBWs98QzZQzeMxZUcBiha8+2YVRDwh5lwCzy0ZuedVenT61EhDqZdOr8MqPFHg==\",\n" +
            "                \"shasum\": \"2e96f125a63dce402b8a6636b68130ed409b06c7\",\n" +
            "                \"tarball\": \"http://localhost:38080/storages/public-project/ohpm-local/@ohos/lottie/-/lottie-2.0.10.har\"\n" +
            "            },\n" +
            "            \"_id\": \"@ohos/lottie@2.0.10\"\n" +
            "        }\n" +
            "    },\n" +
            "    \"maintainers\": [],\n" +
            "    \"time\": {\n" +
            "        \"modified\": \"2024-05-10T18:00:00.118Z\",\n" +
            "        \"created\": \"2024-05-10T18:00:00.118Z\",\n" +
            "        \"2.0.10\": \"2024-05-10T18:00:00.118Z\"\n" +
            "    },\n" +
            "    \"keywords\": [],\n" +
            "    \"_rev\": \"1-2979567f9f807f26\",\n" +
            "    \"_id\": \"@ohos/lottie\"\n" +
            "}";

}
