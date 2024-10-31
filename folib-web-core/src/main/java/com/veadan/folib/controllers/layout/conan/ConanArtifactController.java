package com.veadan.folib.controllers.layout.conan;

import cn.hutool.json.JSONUtil;
import com.alibaba.fastjson.JSONObject;
import com.google.common.collect.Lists;
import com.google.common.collect.Maps;
import com.veadan.folib.artifact.coordinates.ConanArtifactCoordinates;
import com.veadan.folib.artifact.coordinates.ConanArtifactIndex;
import com.veadan.folib.constant.GlobalConstants;
import com.veadan.folib.controllers.BaseArtifactController;
import com.veadan.folib.domain.ConanPackagesRevisions;
import com.veadan.folib.domain.ConanRevisions;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.providers.layout.LayoutFileSystemProvider;
import com.veadan.folib.services.ArtifactIndexService;
import com.veadan.folib.services.ConanService;
import com.veadan.folib.storage.repository.Repository;
import com.veadan.folib.users.security.JwtAuthenticationClaimsProvider;
import com.veadan.folib.users.security.JwtClaimsProvider;
import com.veadan.folib.users.security.SecurityTokenProvider;
import com.veadan.folib.users.userdetails.SpringSecurityUser;
import com.veadan.folib.web.LayoutRequestMapping;
import com.veadan.folib.web.RepositoryMapping;
import io.swagger.annotations.Api;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.codec.digest.MessageDigestAlgorithms;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.collections4.MapUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.security.core.Authentication;
import org.springframework.web.bind.annotation.*;

import javax.inject.Inject;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.BufferedInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.util.*;

@LayoutRequestMapping(ConanArtifactCoordinates.LAYOUT_NAME)
@RestController
@Slf4j
@Api(description = "Conan坐标控制器", tags = "Conan坐标控制器")
public class ConanArtifactController extends BaseArtifactController {

    @Inject
    private SecurityTokenProvider securityTokenProvider;

    @Inject
    @JwtAuthenticationClaimsProvider.JwtAuthentication
    private JwtClaimsProvider jwtClaimsProvider;

    @Inject
    private ArtifactIndexService artifactIndexService;

    @Inject
    private ConanService conanService;

    @Override
    @PreAuthorize("authenticated")
    @GetMapping(value = "/{storageId}/{repositoryId}")
    public ResponseEntity<String> checkRepositoryAccess() {
        return super.checkRepositoryAccess();
    }

    @PreAuthorize("hasAuthority('ARTIFACTS_VIEW')")
    @GetMapping(value = "{storageId}/{repositoryId}/v1/ping")
    public ResponseEntity ping(@RequestHeader HttpHeaders httpHeaders,
                               HttpServletRequest request, HttpServletResponse response) {
        response.setHeader("X-Conan-Server-Version", "0.20.0");
        response.setHeader("X-Conan-Server-Capabilities", "complex_search,checksum_deploy,revisions,matrix_params");
        return new ResponseEntity<>("ok", HttpStatus.OK);
    }

    @PreAuthorize("hasAuthority('ARTIFACTS_RESOLVE')")
    @GetMapping(value = {"{storageId}/{repositoryId}/v1/users/check_credentials", "{storageId}/{repositoryId}/v2/users/check_credentials"})
    public ResponseEntity checkCredentials(Authentication authentication,
                                           @RequestHeader(HttpHeaders.ACCEPT) String accept,
                                           @RequestHeader HttpHeaders httpHeaders,
                                           HttpServletRequest request, HttpServletResponse response) {
        if (Objects.isNull(authentication)) {
            return new ResponseEntity<>(HttpStatus.UNAUTHORIZED.getReasonPhrase(), HttpStatus.UNAUTHORIZED);
        }
        response.setHeader("X-Conan-Server-Version", "0.20.0");
        response.setHeader("X-Conan-Server-Capabilities", "complex_search,checksum_deploy,revisions,matrix_params");
        return new ResponseEntity<>("ok", HttpStatus.OK);
    }

    @PreAuthorize("hasAuthority('ARTIFACTS_RESOLVE')")
    @GetMapping(value = {"{storageId}/{repositoryId}/v1/users/authenticate", "{storageId}/{repositoryId}/v2/users/authenticate"})
    public ResponseEntity userAuthenticate(Authentication authentication,
                                           @RequestHeader(HttpHeaders.ACCEPT) String accept,
                                           @RequestHeader HttpHeaders httpHeaders,
                                           HttpServletRequest request, HttpServletResponse response) throws Exception {

        if (Objects.nonNull(authentication) && authentication.getPrincipal() instanceof SpringSecurityUser) {
            SpringSecurityUser springSecurityUser = (SpringSecurityUser) authentication.getPrincipal();
            Map<String, String> claimMap = jwtClaimsProvider.getClaims(springSecurityUser);
            int expireSeconds = 2626560;
            String token = securityTokenProvider.getToken(springSecurityUser.getUsername(), claimMap, expireSeconds, null);
            return ResponseEntity.ok(token);
        }
        return new ResponseEntity<>("ok", HttpStatus.OK);
    }

    @PreAuthorize("hasAuthority('ARTIFACTS_RESOLVE')")
    @GetMapping(value = "{storageId}/{repositoryId}/v1/conans/search")
    public ResponseEntity search(
            @RepositoryMapping Repository repository,
            @RequestParam(value = "q", required = false) String query) {
        return ResponseEntity.ok(conanService.search("v1", repository, query));
    }

    @PreAuthorize("hasAuthority('ARTIFACTS_RESOLVE')")
    @GetMapping(value = {"{storageId}/{repositoryId}/v1/conans/{name}/{version}/{user}/{channel}/search"})
    public ResponseEntity search(@RepositoryMapping Repository repository,
                                 @PathVariable("storageId") String storageId,
                                 @PathVariable("repositoryId") String repositoryId,
                                 @PathVariable("name") String name,
                                 @PathVariable("version") String version,
                                 @PathVariable("user") String user,
                                 @PathVariable("channel") String channel,
                                 HttpServletRequest request) {
        String targetUrl = getTargetUrl(request.getRequestURI(), "/v1/conans/");
        String artifactPath = String.format("%s/%s/%s/%s/", user, name, version, channel);
        return ResponseEntity.ok(conanService.revisionsSearch(repository, artifactPath, targetUrl));
    }

    @PreAuthorize("hasAuthority('ARTIFACTS_RESOLVE')")
    @GetMapping(value = "{storageId}/{repositoryId}/v2/conans/search")
    public ResponseEntity conanSearch(
            @RepositoryMapping Repository repository,
            @RequestParam(value = "q", required = false) String query) {
        return ResponseEntity.ok(conanService.search("v2", repository, query));
    }

    @PreAuthorize("hasAuthority('ARTIFACTS_RESOLVE')")
    @GetMapping(value = "{storageId}/{repositoryId}/v1/conans/{name}/{version}/{user}/{channel}/digest")
    public ResponseEntity exportDigest(@RepositoryMapping Repository repository,
                                       @PathVariable("name") String name,
                                       @PathVariable("version") String version,
                                       @PathVariable("user") String user,
                                       @PathVariable("channel") String channel) throws IOException {
        JSONObject data = conanService.digest(repository, name, version, user, channel);
        if (Objects.nonNull(data) && !JSONUtil.isNull(data) && data.keySet().size() > 0) {
            return ResponseEntity.ok(data);
        }
        return new ResponseEntity<>(HttpStatus.NOT_FOUND.getReasonPhrase(), HttpStatus.NOT_FOUND);
    }

    @PreAuthorize("hasAuthority('ARTIFACTS_RESOLVE')")
    @GetMapping(value = "{storageId}/{repositoryId}/v1/conans/{name}/{version}/{user}/{channel}/packages/{packageId}/digest")
    public ResponseEntity packagesDigest(@RepositoryMapping Repository repository,
                                         @PathVariable("name") String name,
                                         @PathVariable("version") String version,
                                         @PathVariable("user") String user,
                                         @PathVariable("channel") String channel,
                                         @PathVariable("packageId") String packageId) throws IOException {
        JSONObject data = conanService.packageDigest(repository, name, version, user, channel, packageId);
        if (Objects.nonNull(data) && !JSONUtil.isNull(data) && data.keySet().size() > 0) {
            return ResponseEntity.ok(data);
        }
        return new ResponseEntity<>(HttpStatus.NOT_FOUND.getReasonPhrase(), HttpStatus.NOT_FOUND);
    }

    @PreAuthorize("hasAuthority('ARTIFACTS_RESOLVE')")
    @GetMapping(value = "{storageId}/{repositoryId}/v1/conans/{name}/{version}/{user}/{channel}")
    public ResponseEntity checkExport(@RepositoryMapping Repository repository,
                                      @PathVariable("storageId") String storageId,
                                      @PathVariable("repositoryId") String repositoryId,
                                      @PathVariable("name") String name,
                                      @PathVariable("version") String version,
                                      @PathVariable("user") String user,
                                      @PathVariable("channel") String channel) throws Exception {
        Map<String, String> dataMap = Maps.newHashMap();
        String prefix = String.format("%s/%s/%s/%s/0/export", user, name, version, channel);
        LayoutFileSystemProvider provider = repositoryPathResolver.resolve(storageId, repositoryId).getFileSystem().provider();
        List<String> filenameList = Lists.newArrayList("conan_sources.tgz", "conanfile.py", "conanmanifest.txt");
        filenameList.forEach(filename -> {
            try {
                String artifactPath = String.format("%s/%s", prefix, filename);
                RepositoryPath repositoryPath = repositoryPathResolver.resolve(repository.getStorage().getId(), repository.getId(), artifactPath);
                if (Objects.nonNull(repositoryPath) && Files.exists(repositoryPath)) {
                    final RepositoryPath checksumPath = provider.getChecksumPath(repositoryPath, MessageDigestAlgorithms.MD5);
                    dataMap.put(filename, Files.exists(checksumPath) ? Files.readString(checksumPath) : "");
                }
            } catch (Exception ex) {
                logger.error(ExceptionUtils.getStackTrace(ex));
            }
        });
        if (MapUtils.isEmpty(dataMap)) {
            return new ResponseEntity<>(HttpStatus.NOT_FOUND.getReasonPhrase(), HttpStatus.NOT_FOUND);
        }
        return ResponseEntity.ok(dataMap);
    }

    @PreAuthorize("hasAuthority('ARTIFACTS_DEPLOY')")
    @PostMapping(value = "{storageId}/{repositoryId}/v1/conans/{name}/{version}/{user}/{channel}/upload_urls")
    public ResponseEntity getExportUploadUrls(@RepositoryMapping Repository repository,
                                              @PathVariable("name") String name,
                                              @PathVariable("version") String version,
                                              @PathVariable("user") String user,
                                              @PathVariable("channel") String channel,
                                              @RequestBody(required = false) LinkedHashMap<String, String> obj) {
        if (obj == null) {
            return new ResponseEntity<>("", HttpStatus.NOT_FOUND);
        }

        String url = getRepositoryBaseUrl(repository);
        obj.entrySet().forEach(entry -> {
            String packageName = entry.getKey();
            entry.setValue(String.format("%s/v1/files/%s/%s/%s/%s/0/export/%s", url, user, name, version, channel, packageName));
        });
        return new ResponseEntity<>(obj, HttpStatus.OK);
    }

    @PreAuthorize("hasAuthority('ARTIFACTS_RESOLVE')")
    @GetMapping(value = "{storageId}/{repositoryId}/v1/conans/{name}/{version}/{user}/{channel}/packages/{packageId}")
    public ResponseEntity getPackageInfo(@RepositoryMapping Repository repository,
                                         @PathVariable("name") String name,
                                         @PathVariable("version") String version,
                                         @PathVariable("user") String user,
                                         @PathVariable("channel") String channel,
                                         @PathVariable("packageId") String packageId,
                                         HttpServletRequest request) throws IOException {
        String targetUrl = getTargetUrl(request.getRequestURI(), "/v1/conans/");
        JSONObject data = conanService.getPackageInfo(repository, name, version, user, channel, packageId, targetUrl);
        if (Objects.nonNull(data) && !JSONUtil.isNull(data) && data.keySet().size() > 0) {
            return ResponseEntity.ok(data);
        }
        return new ResponseEntity<>(HttpStatus.NOT_FOUND.getReasonPhrase(), HttpStatus.NOT_FOUND);
    }

    @PreAuthorize("hasAuthority('ARTIFACTS_DEPLOY')")
    @PostMapping(value = "{storageId}/{repositoryId}/v1/conans/{name}/{version}/{user}/{channel}/packages/{packageId}/upload_urls")
    public ResponseEntity getPackagesUploadUrls(@RepositoryMapping Repository repository,
                                                @PathVariable("name") String name,
                                                @PathVariable("version") String version,
                                                @PathVariable("user") String user,
                                                @PathVariable("channel") String channel,
                                                @PathVariable("packageId") String packageId,
                                                @RequestBody(required = false) LinkedHashMap<String, String> obj) {
        String url = getRepositoryBaseUrl(repository);
        obj.entrySet().forEach(entry -> {
            String packageName = entry.getKey();
            entry.setValue(String.format("%s/v1/files/%s/%s/%s/%s/0/package/%s/0/%s", url, user, name, version, channel, packageId, packageName));
        });
        return new ResponseEntity<>(obj, HttpStatus.OK);
    }

    @PreAuthorize("hasAuthority('ARTIFACTS_DEPLOY')")
    @RequestMapping(value = "{storageId}/{repositoryId}/v1/files/{user}/{name}/{version}/{channel}/{revisionId}/export/{filePath:.+}",
            method = {RequestMethod.PUT})
    public ResponseEntity uploadExports(HttpServletRequest request,
                                        @RepositoryMapping Repository repository,
                                        @PathVariable("storageId") String storageId,
                                        @PathVariable("repositoryId") String repositoryId,
                                        @PathVariable("user") String user,
                                        @PathVariable("name") String name,
                                        @PathVariable("version") String version,
                                        @PathVariable("channel") String channel,
                                        @PathVariable("revisionId") String revisionId,
                                        @PathVariable("filePath") String filePath) throws IOException {
        String artifactPath = String.format("%s/%s/%s/%s/%s/export/%s", user, name, version, channel, revisionId, filePath);
        repositoryId=ifIsGroupAndStoreToDefault(repository);
        RepositoryPath repositoryPath = repositoryPathResolver.resolve(storageId, repositoryId, artifactPath);
        InputStream inputStream = request.getInputStream();
        if (inputStream == null || inputStream.available() == 0) {
            String checksumDeploy = request.getHeader("X-Checksum-Deploy"), checksumSha1 = request.getHeader("X-Checksum-Sha1");
            if (Boolean.TRUE.equals(Boolean.valueOf(checksumDeploy)) && StringUtils.isNotBlank(checksumSha1)) {
                if (artifactRealExists(repositoryPath)) {
                    String sha1 = repositoryPath.getArtifactEntry().getChecksums().getOrDefault(MessageDigestAlgorithms.SHA_1, "");
                    if (checksumSha1.equals(sha1)) {
                        return ResponseEntity.status(HttpStatus.CREATED).body("The artifact was exists.");
                    }
                }
            }
            return new ResponseEntity<>(HttpStatus.NOT_FOUND.getReasonPhrase(), HttpStatus.NOT_FOUND);
        }
        try (InputStream bufferedInputStream = new BufferedInputStream(inputStream)) {
            logger.info("Conan v1 upload export storageId [{}] repositoryId [{}] artifactPath [{}]", storageId, repositoryId, artifactPath);
            artifactManagementService.validateAndStore(repositoryPath, bufferedInputStream);
            writeRevisionsIndex(storageId, repositoryId, user, name, version, channel);
            return ResponseEntity.status(HttpStatus.CREATED).body("The artifact was deployed successfully.");
        } catch (Exception e) {
            logger.error(e.getMessage(), e);
            return ResponseEntity.status(HttpStatus.FORBIDDEN).body(e.getMessage());
        }
    }

    @PreAuthorize("hasAuthority('ARTIFACTS_DEPLOY')")
    @RequestMapping(value = "{storageId}/{repositoryId}/v1/files/{user}/{name}/{version}/{channel}/{revisionId}/package/{packageId}/{packageRevisionId}/{filePath:.+}",
            method = {RequestMethod.PUT})
    public ResponseEntity uploadPackages(HttpServletRequest request,
                                         @RepositoryMapping Repository repository,
                                         @PathVariable("storageId") String storageId,
                                         @PathVariable("repositoryId") String repositoryId,
                                         @PathVariable("user") String user,
                                         @PathVariable("name") String name,
                                         @PathVariable("version") String version,
                                         @PathVariable("channel") String channel,
                                         @PathVariable("revisionId") String revisionId,
                                         @PathVariable("packageId") String packageId,
                                         @PathVariable("packageRevisionId") String packageRevisionId,
                                         @PathVariable("filePath") String filePath) throws IOException {
        String artifactPath = String.format("%s/%s/%s/%s/%s/package/%s/%s/%s", user, name, version, channel, revisionId, packageId, packageRevisionId, filePath);
        repositoryId=ifIsGroupAndStoreToDefault(repository);
        RepositoryPath repositoryPath = repositoryPathResolver.resolve(storageId, repositoryId, artifactPath);
        InputStream inputStream = request.getInputStream();
        if (inputStream == null || inputStream.available() == 0) {
            String checksumDeploy = request.getHeader("X-Checksum-Deploy"), checksumSha1 = request.getHeader("X-Checksum-Sha1");
            if (Boolean.TRUE.equals(Boolean.valueOf(checksumDeploy)) && StringUtils.isNotBlank(checksumSha1)) {
                if (artifactRealExists(repositoryPath)) {
                    String sha1 = repositoryPath.getArtifactEntry().getChecksums().getOrDefault(MessageDigestAlgorithms.SHA_1, "");
                    if (checksumSha1.equals(sha1)) {
                        return ResponseEntity.status(HttpStatus.CREATED).body("The artifact was exists.");
                    }
                }
            }
            return new ResponseEntity<>(HttpStatus.NOT_FOUND.getReasonPhrase(), HttpStatus.NOT_FOUND);
        }
        try (InputStream bufferedInputStream = new BufferedInputStream(inputStream)) {
            logger.info("Conan v1 upload package storageId [{}] repositoryId [{}] artifactPath [{}]", storageId, repositoryId, artifactPath);
            artifactManagementService.validateAndStore(repositoryPath, bufferedInputStream);
            writePackagesRevisionsIndex(storageId, repositoryId, user, name, version, channel, revisionId, packageId);
            return ResponseEntity.status(HttpStatus.CREATED).body("The artifact was deployed successfully.");
        } catch (Exception e) {
            logger.error(e.getMessage(), e);
            return ResponseEntity.status(HttpStatus.FORBIDDEN).body(e.getMessage());
        }
    }

    @PreAuthorize("hasAuthority('ARTIFACTS_RESOLVE')")
    @GetMapping(value = {"{storageId}/{repositoryId}/v1/conans/{name}/{version}/{user}/{channel}/download_urls"})
    public ResponseEntity getExportDownloadUrls(@RepositoryMapping Repository repository,
                                                @PathVariable("name") String name,
                                                @PathVariable("version") String version,
                                                @PathVariable("user") String user,
                                                @PathVariable("channel") String channel) throws Exception {
        JSONObject data = conanService.downloadUrls(repository, name, version, user, channel);
        if (Objects.nonNull(data) && !JSONUtil.isNull(data) && data.keySet().size() > 0) {
            return ResponseEntity.ok(data);
        }
        return new ResponseEntity<>(HttpStatus.NOT_FOUND.getReasonPhrase(), HttpStatus.NOT_FOUND);
    }

    @PreAuthorize("hasAuthority('ARTIFACTS_RESOLVE')")
    @GetMapping(value = "{storageId}/{repositoryId}/v1/conans/{name}/{version}/{user}/{channel}/packages/{packageId}/download_urls")
    public ResponseEntity getPackagesDownloadUrls(
            @RepositoryMapping Repository repository,
            @PathVariable("name") String name,
            @PathVariable("version") String version,
            @PathVariable("user") String user,
            @PathVariable("channel") String channel,
            @PathVariable("packageId") String packageId) {
        JSONObject data = conanService.packageDownloadUrls(repository, name, version, user, channel, packageId);
        if (Objects.nonNull(data) && !JSONUtil.isNull(data) && data.keySet().size() > 0) {
            return ResponseEntity.ok(data);
        }
        return new ResponseEntity<>(HttpStatus.NOT_FOUND.getReasonPhrase(), HttpStatus.NOT_FOUND);
    }

    @PreAuthorize("hasAuthority('ARTIFACTS_RESOLVE')")
    @RequestMapping(value = "{storageId}/{repositoryId}/v1/files/{path:.+}",
            method = {RequestMethod.GET})
    public void downloadFiles(@RepositoryMapping Repository repository,
                              @PathVariable("path") String path,
                              @RequestHeader HttpHeaders httpHeaders, HttpServletRequest request, HttpServletResponse response) throws Exception {
        final String storageId = repository.getStorage().getId();
        final String repositoryId = repository.getId();
        logger.info("Requested downloadFiles {}/{}/{}.", storageId, repositoryId, path);
        RepositoryPath repositoryPath = getLocalRepositoryPath(storageId, repositoryId, path);
        if (!Files.exists(repositoryPath)) {
            String targetUrl = getTargetUrl(request.getRequestURI(), "/v1/files/");
            repositoryPath.setTargetUrl(targetUrl);
            repositoryPath = artifactResolutionService.resolvePath(repositoryPath);
            if (repositoryPath == null && path.endsWith("/conan_export.tgz")) {
                path = path.replaceAll("/conan_export.tgz", "/conan_sources.tgz");
                repositoryPath = getLocalRepositoryPath(storageId, repositoryId, path);
                repositoryPath.setTargetUrl(targetUrl);
                repositoryPath = artifactResolutionService.resolvePath(repositoryPath);
            }
        }
        vulnerabilityBlock(repositoryPath);
        response.setCharacterEncoding("UTF-8");
        if (Objects.nonNull(repositoryPath) && Files.exists(repositoryPath)) {
            // 设置文件头：设置下载文件名
            response.setHeader("Content-Disposition", "attachment;" + repositoryPath.getFileName().toString());
        }
        provideArtifactDownloadResponse(request, response, httpHeaders, repositoryPath);
    }

    @PreAuthorize("hasAuthority('ARTIFACTS_RESOLVE')")
    @GetMapping(value = "{storageId}/{repositoryId}/v2/conans/{name}/{version}/{user}/{channel}/{revisions}")
    public ResponseEntity revisions(
            @RepositoryMapping Repository repository,
            @PathVariable("storageId") String storageId,
            @PathVariable("repositoryId") String repositoryId,
            @PathVariable("name") String name,
            @PathVariable("version") String version,
            @PathVariable("user") String user,
            @PathVariable("channel") String channel,
            @PathVariable("revisions") String revisions,
            HttpServletRequest request) throws Exception {
        String artifactPath = String.format("%s/%s/%s/%s/%s", user, name, version, channel, ConanArtifactIndex.INDEX_JSON_NAME);
        String targetUrl = String.format("/v2/conans/%s/%s/%s/%s/revisions", name, version, user, channel);
        JSONObject data = conanService.revisions(repository, artifactPath, targetUrl);
        if (Objects.isNull(data)) {
            return new ResponseEntity<>(errMsg(HttpStatus.NOT_FOUND.value(), HttpStatus.NOT_FOUND.getReasonPhrase()), HttpStatus.NOT_FOUND);
        }
        ConanRevisions conanRevisions = JSONObject.parseObject(data.toJSONString(), ConanRevisions.class);
        if (Objects.isNull(conanRevisions) || CollectionUtils.isEmpty(conanRevisions.getRevisions())) {
            return new ResponseEntity<>(errMsg(HttpStatus.NOT_FOUND.value(), HttpStatus.NOT_FOUND.getReasonPhrase()), HttpStatus.NOT_FOUND);
        }
        if (GlobalConstants.LATEST.equals(revisions)) {
            return new ResponseEntity<>(conanRevisions.getRevisions().get(0), HttpStatus.OK);
        }
        return new ResponseEntity<>(conanRevisions, HttpStatus.OK);
    }

    @PreAuthorize("hasAuthority('ARTIFACTS_RESOLVE')")
    @GetMapping(value = "{storageId}/{repositoryId}/v2/conans/{name}/{version}/{user}/{channel}/revisions/{revisionId}/files")
    public ResponseEntity revisionsFiles(
            @RepositoryMapping Repository repository,
            @PathVariable("storageId") String storageId,
            @PathVariable("repositoryId") String repositoryId,
            @PathVariable("name") String name,
            @PathVariable("version") String version,
            @PathVariable("user") String user,
            @PathVariable("channel") String channel,
            @PathVariable("revisionId") String revisionId,
            HttpServletRequest request,
            HttpServletResponse response) throws Exception {
        String result = "{\"files\":{\"conan_export.tgz\":{},\"conanmanifest.txt\":{},\"conanfile.py\":{}}}";
        response.setContentType(MediaType.APPLICATION_JSON_VALUE);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @PreAuthorize("hasAuthority('ARTIFACTS_RESOLVE')")
    @GetMapping(value = "{storageId}/{repositoryId}/v2/conans/{name}/{version}/{user}/{channel}/revisions/{revisionId}/search")
    public ResponseEntity revisionsSearch(
            @RepositoryMapping Repository repository,
            @PathVariable("storageId") String storageId,
            @PathVariable("repositoryId") String repositoryId,
            @PathVariable("name") String name,
            @PathVariable("version") String version,
            @PathVariable("user") String user,
            @PathVariable("channel") String channel,
            @PathVariable("revisionId") String revisionId,
            HttpServletRequest request,
            HttpServletResponse response) throws Exception {
        String targetUrl = getTargetUrl(request.getRequestURI(), "/v2/conans/");
        String artifactPath = String.format("%s/%s/%s/%s/%s/package", user, name, version, channel, revisionId);
        return ResponseEntity.ok(conanService.revisionsSearch(repository, artifactPath, targetUrl));
    }

    @PreAuthorize("hasAuthority('ARTIFACTS_RESOLVE')")
    @GetMapping(value = "{storageId}/{repositoryId}/v2/conans/{name}/{version}/{user}/{channel}/revisions/{revisionId}/files/{filePath:.+}")
    public void downloadRevisionsFiles(
            @RequestHeader HttpHeaders httpHeaders,
            @RepositoryMapping Repository repository,
            @PathVariable("storageId") String storageId,
            @PathVariable("repositoryId") String repositoryId,
            @PathVariable("name") String name,
            @PathVariable("version") String version,
            @PathVariable("user") String user,
            @PathVariable("channel") String channel,
            @PathVariable("revisionId") String revisionId,
            @PathVariable("filePath") String filePath,
            HttpServletRequest request,
            HttpServletResponse response) throws Exception {
        String artifactPath = String.format("%s/%s/%s/%s/%s/export/%s", user, name, version, channel, revisionId, filePath);
        RepositoryPath repositoryPath = getLocalRepositoryPath(storageId, repositoryId, artifactPath);
        if (!Files.exists(repositoryPath)) {
            String targetUrl = getTargetUrl(request.getRequestURI(), "/v2/conans/");
            repositoryPath.setTargetUrl(targetUrl);
            repositoryPath = artifactResolutionService.resolvePath(repositoryPath);
            if ((Objects.isNull(repositoryPath) || !Files.exists(repositoryPath)) && artifactPath.endsWith("conan_export.tgz")) {
                artifactPath = artifactPath.replace("conan_export.tgz", "conan_sources.tgz");
                repositoryPath = getLocalRepositoryPath(storageId, repositoryId, artifactPath);
                repositoryPath.setTargetUrl(targetUrl);
                repositoryPath = artifactResolutionService.resolvePath(repositoryPath);
            }
        }
        vulnerabilityBlock(repositoryPath);
        provideArtifactDownloadResponse(request, response, httpHeaders, repositoryPath);
    }

    @PreAuthorize("hasAuthority('ARTIFACTS_RESOLVE')")
    @GetMapping(value = "{storageId}/{repositoryId}/v2/conans/{name}/{version}/{user}/{channel}/revisions/{revisionId}/packages/{packageId}/{revisions}")
    public ResponseEntity revisionsPackages(
            @RepositoryMapping Repository repository,
            @PathVariable("storageId") String storageId,
            @PathVariable("repositoryId") String repositoryId,
            @PathVariable("name") String name,
            @PathVariable("version") String version,
            @PathVariable("user") String user,
            @PathVariable("channel") String channel,
            @PathVariable("revisionId") String revisionId,
            @PathVariable("packageId") String packageId,
            @PathVariable("revisions") String revisions) throws Exception {
        String artifactPath = String.format("%s/%s/%s/%s/%s/package/%s/%s", user, name, version, channel, revisionId, packageId, ConanArtifactIndex.INDEX_JSON_NAME);
        String targetUrl = String.format("/v2/conans/%s/%s/%s/%s/revisions/%s/packages/%s/revisions", name, version, user, channel, revisionId, packageId);
        JSONObject data = conanService.revisions(repository, artifactPath, targetUrl);
        if (Objects.isNull(data)) {
            return new ResponseEntity<>(errMsg(HttpStatus.NOT_FOUND.value(), HttpStatus.NOT_FOUND.getReasonPhrase()), HttpStatus.NOT_FOUND);
        }
        ConanPackagesRevisions conanPackagesRevisions = JSONObject.parseObject(data.toJSONString(), ConanPackagesRevisions.class);
        if (CollectionUtils.isEmpty(conanPackagesRevisions.getRevisions())) {
            return new ResponseEntity<>(errMsg(HttpStatus.NOT_FOUND.value(), HttpStatus.NOT_FOUND.getReasonPhrase()), HttpStatus.NOT_FOUND);
        }
        if (GlobalConstants.LATEST.equals(revisions)) {
            return new ResponseEntity<>(conanPackagesRevisions.getRevisions().get(0), HttpStatus.OK);
        }
        return new ResponseEntity<>(conanPackagesRevisions, HttpStatus.OK);
    }


    @PreAuthorize("hasAuthority('ARTIFACTS_RESOLVE')")
    @GetMapping(value = "{storageId}/{repositoryId}/v2/conans/{name}/{version}/{user}/{channel}/revisions/{revisionId}/packages/{packageId}/{revisions}/{packageRevisionId}/files")
    public ResponseEntity packagesFiles(
            @RepositoryMapping Repository repository,
            @PathVariable("storageId") String storageId,
            @PathVariable("repositoryId") String repositoryId,
            @PathVariable("name") String name,
            @PathVariable("version") String version,
            @PathVariable("user") String user,
            @PathVariable("channel") String channel,
            @PathVariable("revisionId") String revisionId,
            @PathVariable("packageId") String packageId,
            @PathVariable("revisions") String revisions,
            @PathVariable("packageRevisionId") String packageRevisionId,
            HttpServletRequest request,
            HttpServletResponse response) throws Exception {
        String result = "{\"files\":{\"conaninfo.txt\":{},\"conan_package.tgz\":{},\"conanmanifest.txt\":{}}}";
        response.setContentType(MediaType.APPLICATION_JSON_VALUE);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @PreAuthorize("hasAuthority('ARTIFACTS_RESOLVE')")
    @GetMapping(value = "{storageId}/{repositoryId}/v2/conans/{name}/{version}/{user}/{channel}/revisions/{revisionId}/packages/{packageId}/{revisions}/{packageRevisionId}/files/{filePath:.+}")
    public void downloadPackagesFiles(
            @RequestHeader HttpHeaders httpHeaders,
            @RepositoryMapping Repository repository,
            @PathVariable("storageId") String storageId,
            @PathVariable("repositoryId") String repositoryId,
            @PathVariable("name") String name,
            @PathVariable("version") String version,
            @PathVariable("user") String user,
            @PathVariable("channel") String channel,
            @PathVariable("revisionId") String revisionId,
            @PathVariable("packageId") String packageId,
            @PathVariable("revisions") String revisions,
            @PathVariable("packageRevisionId") String packageRevisionId,
            @PathVariable("filePath") String filePath,
            HttpServletRequest request,
            HttpServletResponse response) throws Exception {
        String artifactPath = String.format("%s/%s/%s/%s/%s/package/%s/%s/%s", user, name, version, channel, revisionId, packageId, packageRevisionId, filePath);
        RepositoryPath repositoryPath = getLocalRepositoryPath(storageId, repositoryId, artifactPath);
        if (!Files.exists(repositoryPath)) {
            String targetUrl = getTargetUrl(request.getRequestURI(), "/v2/conans/");
            repositoryPath.setTargetUrl(targetUrl);
            repositoryPath = artifactResolutionService.resolvePath(repositoryPath);
        }
        vulnerabilityBlock(repositoryPath);
        provideArtifactDownloadResponse(request, response, httpHeaders, repositoryPath);
    }

    @PreAuthorize("hasAuthority('ARTIFACTS_DEPLOY')")
    @PutMapping(value = "{storageId}/{repositoryId}/v2/conans/{name}/{version}/{user}/{channel}/revisions/{revisionId}/files/{filePath:.+}")
    public ResponseEntity uploadExportsV2(
            @RepositoryMapping Repository repository,
            @PathVariable("storageId") String storageId,
            @PathVariable("repositoryId") String repositoryId,
            @PathVariable("name") String name,
            @PathVariable("version") String version,
            @PathVariable("user") String user,
            @PathVariable("channel") String channel,
            @PathVariable("revisionId") String revisionId,
            @PathVariable("filePath") String filePath, HttpServletRequest request) throws Exception {
        repositoryId=ifIsGroupAndStoreToDefault(repository);
        String artifactPath = String.format("%s/%s/%s/%s/%s/export/%s", user, name, version, channel, revisionId, filePath);
        RepositoryPath repositoryPath = repositoryPathResolver.resolve(storageId, repositoryId, artifactPath);
        InputStream inputStream = request.getInputStream();
        if (inputStream == null || inputStream.available() == 0) {
            String checksumDeploy = request.getHeader("X-Checksum-Deploy"), checksumSha1 = request.getHeader("X-Checksum-Sha1");
            if (Boolean.TRUE.equals(Boolean.valueOf(checksumDeploy)) && StringUtils.isNotBlank(checksumSha1)) {
                if (artifactRealExists(repositoryPath)) {
                    String sha1 = repositoryPath.getArtifactEntry().getChecksums().getOrDefault(MessageDigestAlgorithms.SHA_1, "");
                    if (checksumSha1.equals(sha1)) {
                        return ResponseEntity.status(HttpStatus.CREATED).body("The artifact was exists.");
                    }
                }
            }
            return new ResponseEntity<>(HttpStatus.NOT_FOUND.getReasonPhrase(), HttpStatus.NOT_FOUND);
        }
        try (InputStream bufferedInputStream = new BufferedInputStream(inputStream)) {
            logger.info("Conan v2 upload export storageId [{}] repositoryId [{}] artifactPath [{}]", storageId, repositoryId, artifactPath);
            artifactManagementService.validateAndStore(repositoryPath, bufferedInputStream);
            writeRevisionsIndex(storageId, repositoryId, user, name, version, channel);
            return ResponseEntity.status(HttpStatus.CREATED).body("The artifact was deployed successfully.");
        } catch (Exception e) {
            logger.error(e.getMessage(), e);
            return ResponseEntity.status(HttpStatus.FORBIDDEN).body(e.getMessage());
        }
    }

    @PreAuthorize("hasAuthority('ARTIFACTS_DEPLOY')")
    @PutMapping(value = "{storageId}/{repositoryId}/v2/conans/{name}/{version}/{user}/{channel}/revisions/{revisionId}/packages/{packageId}/revisions/{packageRevisionId}/files/{filePath:.+}")
    public ResponseEntity uploadPackages(
            @RepositoryMapping Repository repository,
            @PathVariable("storageId") String storageId,
            @PathVariable("repositoryId") String repositoryId,
            @PathVariable("name") String name,
            @PathVariable("version") String version,
            @PathVariable("user") String user,
            @PathVariable("channel") String channel,
            @PathVariable("revisionId") String revisionId,
            @PathVariable("packageId") String packageId,
            @PathVariable("packageRevisionId") String packageRevisionId,
            @PathVariable("filePath") String filePath, HttpServletRequest request) throws Exception {
        repositoryId=ifIsGroupAndStoreToDefault(repository);
        String artifactPath = String.format("%s/%s/%s/%s/%s/package/%s/%s/%s", user, name, version, channel, revisionId, packageId, packageRevisionId, filePath);
        RepositoryPath repositoryPath = repositoryPathResolver.resolve(storageId, repositoryId, artifactPath);
        InputStream inputStream = request.getInputStream();
        if (inputStream == null || inputStream.available() == 0) {
            String checksumDeploy = request.getHeader("X-Checksum-Deploy"), checksumSha1 = request.getHeader("X-Checksum-Sha1");
            if (Boolean.TRUE.equals(Boolean.valueOf(checksumDeploy)) && StringUtils.isNotBlank(checksumSha1)) {
                if (artifactRealExists(repositoryPath)) {
                    String sha1 = repositoryPath.getArtifactEntry().getChecksums().getOrDefault(MessageDigestAlgorithms.SHA_1, "");
                    if (checksumSha1.equals(sha1)) {
                        return ResponseEntity.status(HttpStatus.CREATED).body("The artifact was exists.");
                    }
                }
            }
            return new ResponseEntity<>(HttpStatus.NOT_FOUND.getReasonPhrase(), HttpStatus.NOT_FOUND);
        }
        try (InputStream bufferedInputStream = new BufferedInputStream(inputStream)) {
            logger.info("Conan v2 upload package storageId [{}] repositoryId [{}] artifactPath [{}]", storageId, repositoryId, artifactPath);
            artifactManagementService.validateAndStore(repositoryPath, bufferedInputStream);
            writePackagesRevisionsIndex(storageId, repositoryId, user, name, version, channel, revisionId, packageId);
            return ResponseEntity.status(HttpStatus.CREATED).body("The artifact was deployed successfully.");
        } catch (Exception e) {
            logger.error(e.getMessage(), e);
            return ResponseEntity.status(HttpStatus.FORBIDDEN).body(e.getMessage());
        }
    }

    @PreAuthorize("hasAuthority('ARTIFACTS_RESOLVE')")
    @GetMapping(value = "{storageId}/{repositoryId}/{user}/{name}/{version}/{channel}/{revisionId}/package/{packageId}/index.json")
    public void downloadPackageIndexJSON(
            @RepositoryMapping Repository repository,
            @RequestHeader HttpHeaders httpHeaders,
            @PathVariable("storageId") String storageId,
            @PathVariable("repositoryId") String repositoryId,
            @PathVariable("user") String user,
            @PathVariable("name") String name,
            @PathVariable("version") String version,
            @PathVariable("channel") String channel,
            @PathVariable("revisionId") String revisionId,
            @PathVariable("packageId") String packageId,
            HttpServletRequest request,
            HttpServletResponse response) throws Exception {
        String artifactPath = String.format("%s/%s/%s/%s/%s/package/%s/%s", user, name, version, channel, revisionId, packageId, ConanArtifactIndex.INDEX_JSON_NAME);
        RepositoryPath repositoryPath = repositoryPathResolver.resolve(storageId, repositoryId, artifactPath);
        vulnerabilityBlock(repositoryPath);
        provideArtifactDownloadResponse(request, response, httpHeaders, repositoryPath);
    }

    @PreAuthorize("hasAuthority('ARTIFACTS_RESOLVE')")
    @GetMapping(value = "{storageId}/{repositoryId}/{user}/{name}/{version}/{channel}/index.json")
    public void downloadIndexJSON(
            @RepositoryMapping Repository repository,
            @RequestHeader HttpHeaders httpHeaders,
            @PathVariable("storageId") String storageId,
            @PathVariable("repositoryId") String repositoryId,
            @PathVariable("user") String user,
            @PathVariable("name") String name,
            @PathVariable("version") String version,
            @PathVariable("channel") String channel,
            HttpServletRequest request,
            HttpServletResponse response) throws Exception {
        String artifactPath = String.format("%s/%s/%s/%s/%s", user, name, version, channel, ConanArtifactIndex.INDEX_JSON_NAME);
        RepositoryPath repositoryPath = repositoryPathResolver.resolve(storageId, repositoryId, artifactPath);
        vulnerabilityBlock(repositoryPath);
        provideArtifactDownloadResponse(request, response, httpHeaders, repositoryPath);
    }

    @PreAuthorize("hasAuthority('ARTIFACTS_RESOLVE')")
    @RequestMapping(value = {"/{storageId}/{repositoryId}/download/{artifactPath:.+}"}, method = {RequestMethod.GET, RequestMethod.HEAD})
    public void download(@RepositoryMapping Repository repository,
                         @RequestHeader HttpHeaders httpHeaders,
                         @PathVariable("storageId") String storageId,
                         @PathVariable("repositoryId") String repositoryId,
                         @PathVariable("artifactPath") String artifactPath,
                         HttpServletRequest request,
                         HttpServletResponse response)
            throws Exception {
        log.info("Requested download conan {}/{}/{}.", storageId, repositoryId, artifactPath);
        RepositoryPath repositoryPath = repositoryPathResolver.resolve(storageId, repositoryId, artifactPath);
        vulnerabilityBlock(repositoryPath);
        provideArtifactDownloadResponse(request, response, httpHeaders, repositoryPath);
    }

    private Map<String, Object> errMsg(int status, String msg) {
        Map<String, Object> result = new HashMap<>(1);
        Map<String, Object> resultData = new HashMap<>(1);
        resultData.put("status", status);
        resultData.put("message", msg);
        List<Map> list = new ArrayList<>();
        list.add(resultData);
        result.put("errors", list);
        return result;
    }

    private void writeRevisionsIndex(String storageId, String repositoryId, String user, String name, String version, String channel) {
        try {
            String artifactPath = String.format("%s/%s/%s/%s", user, name, version, channel);
            RepositoryPath repositoryPath = repositoryPathResolver.resolve(storageId, repositoryId, artifactPath);
            artifactIndexService.rebuildIndex(repositoryPath);
        } catch (Exception ex) {
            logger.error(ExceptionUtils.getStackTrace(ex));
        }
    }

    private void writePackagesRevisionsIndex(String storageId, String repositoryId, String user, String name, String version, String channel, String revisionId, String packageId) {
        try {
            String artifactPath = String.format("%s/%s/%s/%s/%s/package/%s", user, name, version, channel, revisionId, packageId);
            RepositoryPath repositoryPath = repositoryPathResolver.resolve(storageId, repositoryId, artifactPath);
            artifactIndexService.rebuildIndex(repositoryPath);
        } catch (Exception ex) {
            logger.error(ExceptionUtils.getStackTrace(ex));
        }
    }

    private RepositoryPath getLocalRepositoryPath(String storageId, String repositoryId, String artifactPath) throws IOException {
        RepositoryPath repositoryPath = repositoryPathResolver.resolve(storageId, repositoryId, artifactPath);
        repositoryPath.setDisableRemote(Boolean.TRUE);
        RepositoryPath cacheRepositoryPath = artifactResolutionService.resolvePath(repositoryPath);
        if (Objects.nonNull(cacheRepositoryPath) && Files.exists(cacheRepositoryPath)) {
            return cacheRepositoryPath;
        }
        repositoryPath.setDisableRemote(null);
        return repositoryPath;
    }

    private String getTargetUrl(String uri, String prefix) {
        return uri.substring(uri.indexOf(prefix));
    }
}
