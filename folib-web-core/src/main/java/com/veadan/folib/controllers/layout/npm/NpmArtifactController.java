package com.veadan.folib.controllers.layout.npm;

import com.alibaba.fastjson.JSONObject;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.google.common.collect.Lists;
import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.veadan.folib.annotation.AuditLog;
import com.veadan.folib.artifact.coordinates.NpmArtifactCoordinates;
import com.veadan.folib.authentication.api.password.PasswordAuthentication;
import com.veadan.folib.components.NpmComponent;
import com.veadan.folib.config.NpmLayoutProviderConfig.NpmObjectMapper;
import com.veadan.folib.constant.GlobalConstants;
import com.veadan.folib.controllers.BaseArtifactController;
import com.veadan.folib.data.criteria.Paginator;
import com.veadan.folib.enums.AuditEventNameEnum;
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
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.providers.layout.*;
import com.veadan.folib.providers.repository.RepositoryProvider;
import com.veadan.folib.providers.repository.RepositoryProviderRegistry;
import com.veadan.folib.providers.repository.RepositorySearchRequest;
import com.veadan.folib.repository.NpmRepositoryFeatures.SearchPackagesEventListener;
import com.veadan.folib.repository.NpmRepositoryFeatures.ViewPackageEventListener;
import com.veadan.folib.services.NpmService;
import com.veadan.folib.storage.repository.Repository;
import com.veadan.folib.storage.validation.artifact.ArtifactCoordinatesValidationException;
import com.veadan.folib.users.service.UserService;
import com.veadan.folib.users.service.impl.RelationalDatabaseUserService;
import com.veadan.folib.users.userdetails.SpringSecurityUser;
import com.veadan.folib.web.LayoutRequestMapping;
import com.veadan.folib.web.RepositoryMapping;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;
import org.apache.commons.codec.digest.DigestUtils;
import org.apache.commons.io.FilenameUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.javatuples.Pair;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.scheduling.concurrent.ThreadPoolTaskExecutor;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.security.authentication.AuthenticationManager;
import org.springframework.security.authentication.BadCredentialsException;
import org.springframework.security.authentication.InsufficientAuthenticationException;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.Authentication;
import org.springframework.ui.ModelMap;
import org.springframework.web.bind.annotation.*;

import javax.inject.Inject;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.*;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
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
    private NpmService npmService;

    @Inject
    private NpmComponent npmComponent;

    @Inject
    protected ApplicationEventPublisher eventPublisher;

    @Inject
    @RelationalDatabaseUserService.RelationalDatabase
    private UserService userService;

    @Inject
    private ThreadPoolTaskExecutor asyncFetchRemotePackageThreadPoolTaskExecutor;

    @Inject
    private AuthenticationManager authenticationManager;

    @Override
    @PreAuthorize("authenticated")
    @GetMapping(value = {"/api/npm/{repositoryId}/", "/api/ohpm/{repositoryId}/"})
    public ResponseEntity<String> checkRepositoryAccess() {
        return super.checkRepositoryAccess();
    }

    @GetMapping(path = "/api/npm/{repositoryId}/-/v1/search")
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

    @GetMapping(path = "/api/npm/{repositoryId}/-/binary/{artifactPath:.+}")
    @PreAuthorize("hasAuthority('ARTIFACTS_RESOLVE')")
    public void viewBinaryFeedWithScope(@RepositoryMapping Repository repository,
                                        @PathVariable(name = "repositoryId") String repositoryId,
                                        @PathVariable(name = "artifactPath") String artifactPath,
                                        HttpServletRequest request,
                                        HttpServletResponse response,
                                        @RequestHeader HttpHeaders httpHeaders)
            throws Exception {
        final String storageId = repository.getStorage().getId();
        String extension = FilenameUtils.getExtension(artifactPath);
        if (StringUtils.isNotBlank(extension)) {
            String prefix = String.format("/artifactory/api/npm/%s", repositoryId);
            String packageId = request.getRequestURI().substring(prefix.length() + 1);
            RepositoryPath repositoryPath = artifactResolutionService.resolvePath(storageId, repositoryId, packageId);
            vulnerabilityBlock(repositoryPath);
            provideArtifactDownloadResponse(request, response, httpHeaders, repositoryPath);
            return;
        }
        long startTime = System.currentTimeMillis();
        response.setContentType(MediaType.APPLICATION_JSON_VALUE);
        String prefix = String.format("/artifactory/api/npm/%s", repositoryId);
        String packageId = request.getRequestURI().substring(prefix.length());
        String binary = "/binary/";
        String packageName = "binary";
        if (!packageId.endsWith(binary) && !packageId.endsWith(StringUtils.removeEnd(binary, GlobalConstants.SEPARATOR))) {
            packageName = StringUtils.removeEnd(packageId.substring(packageId.indexOf(binary) + binary.length()), GlobalConstants.SEPARATOR);
        }
        String binaryFeed = npmService.binary(repository, packageName, packageId);
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

    @GetMapping(path = {"/api/npm/{repositoryId}/{packageName:[^@^-].*}/{packageVersion}", "/api/ohpm/{repositoryId}/{packageName:[^@^-].*}/{packageVersion}"})
    @PreAuthorize("hasAuthority('ARTIFACTS_RESOLVE')")
    public void viewPackage(@RepositoryMapping Repository repository,
                                     @PathVariable(name = "packageName") String packageName,
                                     @PathVariable(name = "packageVersion") String packageVersion,
                                     HttpServletResponse response)
            throws Exception {
        viewPackageWithScope(repository, null, packageName, packageVersion, response);
    }

    @GetMapping(path = {"/api/npm/{repositoryId}/{packageScope:@.*}/{packageName:[^@^-].*}/{packageVersion}", "/api/ohpm/{repositoryId}/{packageScope:@.*}/{packageName:[^@^-].*}/{packageVersion}"})
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
        final String packageSuffix = NpmSubLayout.OHPM.getValue().equals(repository.getSubLayout()) ? NpmPacketSuffix.HAR.getValue() : NpmPacketSuffix.TGZ.getValue();
        NpmArtifactCoordinates c = NpmArtifactCoordinates.of(packageId, packageVersion, packageSuffix);

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

    @GetMapping(path = {"/api/npm/{repositoryId}/{packageScope:@.*}/{packageName:[^@^-].*}", "/api/ohpm/{repositoryId}/{packageScope:@.*}/{packageName:[^@^-].*}"})
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
        String packageId = NpmArtifactCoordinates.calculatePackageId(packageScope, packageName);
        PackageFeed packageFeed = npmService.packageFeed(repository, packageId, packageId);
        if (Objects.isNull(packageFeed)) {
            String msg = "{\"error\":\"[NOT_FOUND] %s not found\"}";
            response.setStatus(HttpStatus.NOT_FOUND.value());
            response.getOutputStream().write(String.format(msg, packageId).getBytes());
            return;
        }

        try (InputStream inputStream = new ByteArrayInputStream(npmJacksonMapper.writeValueAsBytes(packageFeed))) {
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

    @GetMapping(path = {"/api/npm/{repositoryId}/{packageName:[^@^-].*}", "/api/ohpm/{repositoryId}/{packageName:[^@^-].*}"})
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
                                                          String packageName, String subLayout) {
        List<String> coordinateValues = NpmSubLayout.OHPM.getValue().equals(subLayout) ? Lists.newArrayList("har") : Lists.newArrayList("tgz");
        RepositorySearchRequest rootPredicate = new RepositorySearchRequest(
                NpmArtifactCoordinates.calculatePackageId(packageScope, packageName), Lists.newArrayList(coordinateValues));

        return rootPredicate;
    }

    @PreAuthorize("hasAuthority('ARTIFACTS_RESOLVE')")
    @AuditLog(value = AuditEventNameEnum.DOWNLOAD_EXCEPTION, target = "#repository.getStorage().getId() + '/' + #repository.getId() + '/' + #packageScope + '/' + #packageName + '/-/' + #packageNameWithVersion + '.' + #packageExtension")
    @RequestMapping(path = {"/api/npm/{repositoryId}/{packageScope:@.*}/{packageName:[^@^-].*}/-/{packageNameWithVersion}.{packageExtension}", "/api/ohpm/{repositoryId}/{packageScope:@.*}/{packageName:[^@^-].*}/-/{packageNameWithVersion}.{packageExtension}"},
            method = {RequestMethod.GET, RequestMethod.HEAD})
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
        boolean isPackage = NpmSubLayout.OHPM.getValue().equals(repository.getSubLayout()) ? packageNameWithVersion.startsWith("oh-package") && packageExtension.endsWith("json5") : packageNameWithVersion.startsWith("package-") && packageExtension.endsWith("json");
        String artifactPath = "";
        if (!isPackage) {
            if (!packageNameWithVersion.startsWith(packageName + "-")) {
                response.setStatus(HttpServletResponse.SC_BAD_REQUEST);
                return null;
            }
            packageVersion = getPackageVersion(packageNameWithVersion, packageName);
            NpmArtifactCoordinates coordinates;
            try {
                final String packageSuffix = NpmSubLayout.OHPM.getValue().equals(repository.getSubLayout()) ? NpmPacketSuffix.HAR.getValue() : NpmPacketSuffix.TGZ.getValue();
                coordinates = NpmArtifactCoordinates.of(String.format("%s/%s", packageScope, packageName), packageVersion, packageSuffix);
                artifactPath = coordinates.buildPath();
            } catch (IllegalArgumentException e) {
                response.setStatus(HttpStatus.BAD_REQUEST.value());
                response.getWriter().write(e.getMessage());
                return null;
            }
            RepositoryPath repositoryPath = artifactResolutionService.resolvePath(storageId, repositoryId, artifactPath);
            vulnerabilityBlock(repositoryPath);
            provideArtifactDownloadResponse(request, response, httpHeaders, repositoryPath);
            logger.debug("[{}] downloadPackageWithScope [{}] task time [{}] ms", this.getClass().getSimpleName(), artifactPath, System.currentTimeMillis() - startTime);
        } else {
            packageVersion = getPackageJsonVersion(packageNameWithVersion, repository.getSubLayout());
            String pgName = NpmSubLayout.OHPM.getValue().equals(repository.getSubLayout()) ? NpmLayoutProvider.OH_PACKAGE_JSON : NpmLayoutProvider.PACKAGE_JSON;
            artifactPath = String.format("%s/%s/%s/%s", packageScope, packageName, packageVersion, pgName);
            RepositoryPath repositoryPath = artifactResolutionService.resolvePath(storageId, repositoryId, artifactPath);
            vulnerabilityBlock(repositoryPath);
            String packages = npmComponent.readBinary(repositoryPath);
            GsonBuilder gsonBuilder = new GsonBuilder();
            gsonBuilder.setPrettyPrinting();
            Gson gson = gsonBuilder.create();
            return ResponseEntity.ok(gson.toJson(packages));
        }
        return null;
    }

    @PreAuthorize("hasAuthority('ARTIFACTS_RESOLVE')")
    @AuditLog(value = AuditEventNameEnum.DOWNLOAD_EXCEPTION, target = "#repository.getStorage().getId() + '/' + #repository.getId() + '/' + #packageName + '/-/' + #packageNameWithVersion + '.' + #packageExtension")
    @GetMapping(path = {"/api/npm/{repositoryId}/{packageName:[^@^-].*}/-/{packageNameWithVersion}.{packageExtension}", "/api/ohpm/{repositoryId}/{packageName:[^@^-].*}/-/{packageNameWithVersion}.{packageExtension}"})
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
                final String packageSuffix = NpmSubLayout.OHPM.getValue().equals(repository.getSubLayout()) ? NpmPacketSuffix.HAR.getValue() : NpmPacketSuffix.TGZ.getValue();
                coordinates = NpmArtifactCoordinates.of(packageName, packageVersion, packageSuffix);
            } catch (IllegalArgumentException e) {
                response.setStatus(HttpStatus.BAD_REQUEST.value());
                response.getWriter().write(e.getMessage());
                return null;
            }

            RepositoryPath path = artifactResolutionService.resolvePath(storageId, repositoryId, coordinates.buildPath());
            vulnerabilityBlock(path);
            provideArtifactDownloadResponse(request, response, httpHeaders, path);
            logger.debug("[{}] downloadPackage [{}] task time [{}] ms", this.getClass().getSimpleName(), coordinates.buildPath(), System.currentTimeMillis() - startTime);
        } else {
            packageVersion = getPackageJsonVersion(packageNameWithVersion, repository.getSubLayout());
            artifactPath = String.format("%s/%s/%s/%s", packageName, packageName, packageVersion, NpmLayoutProvider.PACKAGE_JSON);
            RepositoryPath repositoryPath = artifactResolutionService.resolvePath(storageId, repositoryId, artifactPath);
            vulnerabilityBlock(repositoryPath);
            String packages = npmComponent.readBinary(repositoryPath);
            GsonBuilder gsonBuilder = new GsonBuilder();
            gsonBuilder.setPrettyPrinting();
            Gson gson = gsonBuilder.create();
            return ResponseEntity.ok(gson.toJson(packages));
        }
        return null;
    }

//    @PreAuthorize("hasAuthority('ARTIFACTS_RESOLVE')")
//    @AuditLog(value = AuditEventNameEnum.DOWNLOAD_EXCEPTION, target = "#repository.getStorage().getId() + '/' + #repository.getId() + '/' + #packageScope + '/' + #packageName + '/' +#packageVersion '/' + #fileName + '.' + #fileExtension")
//    @GetMapping(path = {"/api/npm/{repositoryId}/{packageScope}/{packageName}/{packageVersion}/{fileName}.{fileExtension}", "/api/ohpm/{repositoryId}/{packageScope}/{packageName}/{packageVersion}/{fileName}.{fileExtension}"})
//    public void downloadPackageWithScopeFile(@RepositoryMapping Repository repository,
//                                             @PathVariable(name = "packageScope") String packageScope,
//                                             @PathVariable(name = "packageName") String packageName,
//                                             @PathVariable(name = "packageVersion") String packageVersion,
//                                             @PathVariable(name = "fileName") String fileName,
//                                             @PathVariable(name = "fileExtension") String fileExtension,
//                                             @RequestHeader HttpHeaders httpHeaders,
//                                             HttpServletRequest request,
//                                             HttpServletResponse response)
//            throws Exception {
//        final String storageId = repository.getStorage().getId();
//        final String repositoryId = repository.getId();
//        String path = packageScope + File.separator + packageName + File.separator + packageVersion + File.separator + fileName + GlobalConstants.POINT + fileExtension;
//        logger.info("Requested /{}/{}/{}.", storageId, repositoryId, path);
//
//        RepositoryPath repositoryPath = artifactResolutionService.resolvePath(storageId, repositoryId, path);
//        vulnerabilityBlock(repositoryPath);
//        provideArtifactDownloadResponse(request, response, httpHeaders, repositoryPath);
//    }

    @PreAuthorize("hasAuthority('ARTIFACTS_DEPLOY')")
    @PutMapping(path = {"/api/npm/{repositoryId}/{name:.+}", "/api/ohpm/{repositoryId}/{name:.+}"}, consumes = MediaType.APPLICATION_JSON_VALUE)
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
            packageEntry = npmComponent.extractPackage(name, request.getInputStream(), subLayout);
        } catch (IllegalArgumentException e) {
            logger.error("Failed to extract npm package data", e);
            return ResponseEntity.badRequest().build();
        }

        final String packageSuffix = NpmSubLayout.OHPM.getValue().equals(subLayout) ? NpmPacketSuffix.HAR.getValue() : NpmPacketSuffix.TGZ.getValue();
        PackageVersion packageJson = packageEntry.getValue0();
        Path packageTgz = packageEntry.getValue1();
        NpmArtifactCoordinates coordinates = NpmArtifactCoordinates.of(name, packageJson.getVersion(), packageSuffix);
        storeNpmPackage(repository, coordinates, packageJson, packageTgz, repository.getSubLayout());
        if (NpmSubLayout.OHPM.getValue().equals(repository.getSubLayout())) {
            OhpmPublishRes res = OhpmPublishRes.builder()
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
     * @param name name from path "/{repositoryId}/{name:.+}"
     * @return true if contains, false if not. If true PUT stage of 'unpublish' will be skipped.
     */
    private boolean nameContainsRevision(String name) {
        if (name.contains("/-rev/")) {
            logger.warn("Url comprises '/-rev/' sub path");

            return true;
        }
        return false;
    }

    @PreAuthorize("hasAuthority('ARTIFACTS_RESOLVE')")
    @PutMapping(path = "/api/npm/{repositoryId}/-/user/org.couchdb.user:{username}",
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

    @DeleteMapping(path = "/api/npm/{repositoryId}/{packageScope:@.*}/{packageName:[^@^-].*}/-rev/{rev}")
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
    @DeleteMapping(path = "/api/npm/{repositoryId}/{r1}/{r2}/{r3}/{packageScope:@.*}/{packageName:[^@^-].*}/-/{tarball}/-rev/{rev}")
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
    @DeleteMapping(path = "/api/npm/{repositoryId}/{packageScope:@.*}/{packageName:[^@^-].*}/-/{tarball}/-rev/{rev}")
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

    @DeleteMapping(path = "/api/npm/{repositoryId}/{packageName:[^@^-].*}/-rev/{rev}")
    @PreAuthorize("hasAuthority('ARTIFACTS_DELETE')")
    public ResponseEntity unpublishPackage(@RepositoryMapping Repository repository,
                                           @PathVariable(name = "packageName") String packageName,
                                           @PathVariable(name = "rev") String rev) {
        return unpublishPackageWithScope(repository, null, packageName, rev);
    }

    @DeleteMapping(path = "/api/npm/{repositoryId}/{r1}/{r2}/{r3}/{packageName:[^@^-].*}/-/{tarball}/-rev/{rev}")
    @PreAuthorize("hasAuthority('ARTIFACTS_DELETE')")
    public ResponseEntity unpublishVersion(@RepositoryMapping Repository repository,
                                           @PathVariable(name = "packageName") String packageName,
                                           @PathVariable(name = "tarball") String tarball,
                                           @PathVariable(name = "rev") String rev) {
        return unpublishVersionWithScope(repository, null, packageName, tarball, rev);
    }

    @DeleteMapping(path = "/api/npm/{repositoryId}/{packageName:[^@^-].*}/-/{tarball}/-rev/{rev}")
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
    @PostMapping(path = "/api/ohpm/{repositoryId}/login")
    public ResponseEntity<?> ohpmLogin(@PathVariable(name = "repositoryId") String repositoryId,
                                       @RequestBody OhpmLoginReq ohpmLoginReq) {
        if (ohpmLoginReq.getPublishId() != null) {
            JSONObject data = new JSONObject();
            String publishId = ohpmLoginReq.getPublishId();
            byte[] decoded = Base64.getDecoder().decode(publishId);
            String basic = new String(decoded, StandardCharsets.UTF_8);
            String[] accountArr = basic.split(":");
            if (accountArr.length != 2) {
                data.put("success", false);
                data.put("error", "The username or password is null!");
                return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(data);
            }
            String username = accountArr[0], password = accountArr[1];
            try {
                authenticationManager.authenticate(new PasswordAuthentication(username, password));
                String token = userService.generateSecurityToken(username, 7200);
                OhpmLoginRes ohpmLoginRes = OhpmLoginRes.builder()
                        .success(true)
                        .token("Bearer " + token)
                        .message("")
                        .build();
                return ResponseEntity.ok(ohpmLoginRes);
            } catch (Exception e) {
                logger.error(ExceptionUtils.getStackTrace(e));
                if (e instanceof BadCredentialsException) {
                    data.put("success", false);
                    data.put("error", "The username or password is invalid!");
                    return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(data);
                }
                return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).build();
            }

        }
        return ResponseEntity.status(HttpStatus.UNAUTHORIZED).build();
    }

    private void storeNpmPackage(Repository repository,
                                 NpmArtifactCoordinates coordinates,
                                 PackageVersion packageDef,
                                 Path packageTgzTmp, String npmSubLayout)
            throws IOException,
            ProviderImplementationException,
            NoSuchAlgorithmException,
            ArtifactCoordinatesValidationException {
        RepositoryPath repositoryPath = repositoryPathResolver.resolve(repository, coordinates);
        try (InputStream is = new BufferedInputStream(Files.newInputStream(packageTgzTmp))) {
            artifactManagementService.validateAndStore(repositoryPath, is);
        }
        Path packageJsonTmp = npmComponent.extractPackageJson(packageTgzTmp, npmSubLayout, packageDef);
        String packageName = NpmSubLayout.OHPM.getValue().equals(npmSubLayout) ? "oh-package.json5" : "package.json";
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
        try ( ByteArrayInputStream is = new ByteArrayInputStream(shasum.getBytes(StandardCharsets.UTF_8))){
            artifactManagementService.validateAndStore(checksumPath,is);
        }


        Files.delete(packageTgzTmp);
        Files.delete(packageJsonTmp);

    }

    private String getPackageVersion(String packageNameWithVersion,
                                     String packageName) {
        return packageNameWithVersion.substring(packageName.length() + 1);
    }

    private String getPackageJsonVersion(String packageJsonNameWithVersion, String subLayout) {
        if (NpmSubLayout.OHPM.getValue().equals(subLayout)) {
            return packageJsonNameWithVersion.substring("oh-package".length() + 1);
        }
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

    @Override
    @ApiOperation(value = "Used to retrieve an artifact")
    @ApiResponses(value = {@ApiResponse(code = 200, message = ""),
            @ApiResponse(code = 400, message = "An error occurred.")})
    @PreAuthorize("hasAuthority('ARTIFACTS_RESOLVE')")
    @AuditLog(value = AuditEventNameEnum.DOWNLOAD_EXCEPTION, target = "#repository.getStorage().getId() + '/' + #repository.getId() + '/' + #path")
    @GetMapping("/{repositoryId:^(?!api$).+}/{path:.+}")
    public Object download(@RepositoryMapping Repository repository,
                           @RequestHeader HttpHeaders httpHeaders,
                           @PathVariable String path,
                           HttpServletRequest request,
                           HttpServletResponse response,
                           ModelMap model)
            throws Exception {
        return super.download(repository, httpHeaders, path, request, response, model);
    }
}
