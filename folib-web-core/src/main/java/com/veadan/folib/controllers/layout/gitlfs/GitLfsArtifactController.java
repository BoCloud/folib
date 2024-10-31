package com.veadan.folib.controllers.layout.gitlfs;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.veadan.folib.config.GitLfsLayoutProviderConfig.GitLfsObjectMapper;
import com.veadan.folib.controllers.BaseArtifactController;
import com.veadan.folib.domain.gitls.model.*;
import com.veadan.folib.domain.gitls.service.GitLfsLocalService;
import com.veadan.folib.model.request.GitLfsBatchReq;
import com.veadan.folib.model.response.GitLfsBatchRes;

import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.providers.layout.GitLfsLayoutProvider;
import com.veadan.folib.storage.repository.Repository;
import com.veadan.folib.users.userdetails.SpringSecurityUser;
import com.veadan.folib.web.LayoutRequestMapping;
import com.veadan.folib.web.RepositoryMapping;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.web.bind.annotation.*;

import javax.annotation.Resource;
import javax.inject.Inject;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.ws.rs.*;

import java.io.IOException;
import java.io.InputStream;
import java.util.List;
import java.util.stream.Collectors;


@RestController
@LayoutRequestMapping(GitLfsLayoutProvider.ALIAS)
@Api(value = "gitlfs坐标控制器", tags = "gitlfs坐标控制器")
public class GitLfsArtifactController extends BaseArtifactController {

    private static final String UPLOAD = "upload";
    private static final String DOWNLOAD = "download";

    @Inject
    @GitLfsObjectMapper
    private ObjectMapper gitLfsObjectMapper;
    @Resource
    private GitLfsLocalService gitLfsLocalService;


    @ApiResponses(value = {@ApiResponse(code = 200, message = "The artifact was deployed successfully."),
            @ApiResponse(code = 400, message = "An error occurred.")})
    @PreAuthorize("hasAuthority('ARTIFACTS_DEPLOY')")
    @GetMapping(value = "{storageId}/{repositoryId}/objects/{OID}", produces = {"application/vnd.git-lfs+json"})
    public ResponseEntity<?> download(@RepositoryMapping Repository repository,
                                      HttpServletRequest request,
                                      @PathVariable("OID") String oid) throws IOException {
        return gitLfsLocalService.lfsDownloadResponse(repository.getStorage().getId(),repository.getId(), oid, request.getHeader("Authorization"));
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = "The artifact was deployed successfully."),
            @ApiResponse(code = 400, message = "An error occurred.")})
    @PreAuthorize("hasAuthority('ARTIFACTS_DEPLOY')")
    @PostMapping(value = "{storageId}/{repositoryId}/objects/{OID}", produces = {"application/vnd.git-lfs+json"})
    public ResponseEntity<?> verify(@RepositoryMapping Repository repository,@PathVariable("OID") String oid) {
        return gitLfsLocalService.lfsVerifyObject(repository.getStorage().getId(),repository.getId(), oid);
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = "The artifact was deployed successfully."),
            @ApiResponse(code = 400, message = "An error occurred.")})
    @PreAuthorize("hasAuthority('ARTIFACTS_DEPLOY')")
    @PostMapping(value = "{storageId}/{repositoryId}/objects", produces = {"application/vnd.git-lfs+json"},consumes = {"application/vnd.git-lfs+json", "application/json"})
    public ResponseEntity<?> upload(@RepositoryMapping Repository repository,
                           HttpServletRequest request) {
        try {
            GitLfsJson gitLfsJson =gitLfsObjectMapper.readValue(request.getInputStream(), GitLfsJson.class);
            return gitLfsLocalService.lfsUploadResponse(repository.getStorage().getId(),repository.getId(), gitLfsJson, request.getHeader("Authorization"));
        } catch (Exception e) {
            this.logger.error("Failed to parse request body into GitLfsBatchJson with an error: {}", e.getMessage());
            this.logger.debug("", e);
            return ResponseEntity.status(HttpStatus.BAD_REQUEST).body("Bad Request");
        }
    }
    @ApiOperation(value = "The Batch API is used to request the ability to transfer LFS objects with the LFS server.")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "The artifact was deployed successfully."),
            @ApiResponse(code = 400, message = "An error occurred.")})
    @PreAuthorize("hasAuthority('ARTIFACTS_DEPLOY')")
    @PostMapping(value = "{storageId}/{repositoryId}/objects/batch", consumes = "application/vnd.git-lfs+json; charset=utf-8")
    public ResponseEntity<?> batch(@RepositoryMapping Repository repository,
                                   @RequestBody GitLfsBatchReq req,
                                   HttpServletRequest request) {

        final String storageId = repository.getStorage().getId();
        final String repositoryId = repository.getId();
        logger.info("Requested /{}/{}.", storageId, repositoryId);
        String auth = request.getHeader(HttpHeaders.AUTHORIZATION);
        GitLfsBatchRes res = UPLOAD.equals(req.getOperation()) ? setUploadRes(req, storageId, repositoryId, auth) : setDownloadRes(req, storageId, repositoryId);
        return ResponseEntity.ok(res);
    }

    @ApiOperation(value = "Used to deploy an artifact")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "The artifact was deployed successfully."),
            @ApiResponse(code = 400, message = "An error occurred.")})
    @PreAuthorize("hasAuthority('ARTIFACTS_DEPLOY')")
    @PutMapping(value = "{storageId}/{repositoryId}/{path:.+}", consumes = "application/*")
    public ResponseEntity<?> upload(@RepositoryMapping Repository repository,
                                    @PathVariable String path,
                                    HttpServletRequest request) {
        final String storageId = repository.getStorage().getId();
        final String repositoryId = repository.getId();

        try (InputStream is =  request.getInputStream()){
            RepositoryPath repositoryPath = repositoryPathResolver.resolve(storageId, repositoryId, path);
            artifactManagementService.validateAndStore(repositoryPath, is);

            return ResponseEntity.ok("The artifact was deployed successfully.");
        } catch (Exception e) {
            logger.error(e.getMessage(), e);

            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).body(e.getMessage());
        }
    }

    @ApiOperation(value = "Used to retrieve an artifact")
    @ApiResponses(value = {@ApiResponse(code = 200, message = ""),
            @ApiResponse(code = 400, message = "An error occurred.")})
    @PreAuthorize("hasAuthority('ARTIFACTS_RESOLVE')")
    @GetMapping(value = {"{storageId}/{repositoryId}/{path:.+}"}, produces = MediaType.APPLICATION_OCTET_STREAM_VALUE)
    public void download(@RepositoryMapping Repository repository,
                         @RequestHeader HttpHeaders httpHeaders,
                         @PathVariable String path,
                         HttpServletRequest request,
                         HttpServletResponse response)
            throws Exception {
        final String storageId = repository.getStorage().getId();
        final String repositoryId = repository.getId();
        logger.info("Requested /{}/{}/{}.", storageId, repositoryId, path);

        RepositoryPath repositoryPath = artifactResolutionService.resolvePath(storageId, repositoryId, path);
        vulnerabilityBlock(repositoryPath);
        provideArtifactDownloadResponse(request, response, httpHeaders, repositoryPath);
    }

    @ApiOperation(value = "Used to retrieve an artifact")
    @ApiResponses(value = {@ApiResponse(code = 200, message = ""),
            @ApiResponse(code = 400, message = "An error occurred.")})
    @PreAuthorize("hasAuthority('ARTIFACTS_DEPLOY')")
    @PostMapping(value = {"{storageId}/{repositoryId}/locks/verify"}, produces = "application/vnd.git-lfs+json", consumes = {"application/vnd.git-lfs+json", "application/json"})
    public ResponseEntity<?> verify(@RepositoryMapping Repository repository,
                                    @RequestHeader HttpHeaders httpHeaders,
                                    HttpServletRequest request,
                                    HttpServletResponse response) {
        final String storageId = repository.getStorage().getId();
        final String repositoryId = repository.getId();
        GitLfsLocksVerification locksVerificationRequest;
        try {
            locksVerificationRequest = gitLfsObjectMapper.readValue(request.getInputStream(), GitLfsLocksVerification.class);
        } catch (Exception e) {
            this.logger.error("Failed to parse request body into GitLfsCreateLockJson with an error: {}", e.getMessage());
            this.logger.debug("", e);
            return ResponseEntity.status(400).body("Bad Request");
        }
        return ResponseEntity.ok(gitLfsLocalService.listLocksForVerification(storageId, repositoryId, locksVerificationRequest));

    }

    @ApiOperation(value = "The client sends the following to create a lock by sending a POST to /locks (appended to the LFS server url, as described above). Servers should ensure that users have push access to the repository, and that files are locked exclusively to one user.")
    @ApiResponses(value = {@ApiResponse(code = 200, message = ""),
            @ApiResponse(code = 400, message = "An error occurred.")})
    @PreAuthorize("hasAuthority('ARTIFACTS_DEPLOY')")
    @PostMapping(value = "{storageId}/{repositoryId}/locks", produces = "application/vnd.git-lfs+json", consumes = {"application/vnd.git-lfs+json", "application/json"})
    public ResponseEntity<?> createLock(@RepositoryMapping Repository repository,
                                        @RequestHeader HttpHeaders httpHeaders,
                                        HttpServletRequest request,
                                        HttpServletResponse response) {
        GitLfsCreateLock createLockJson = null;
        try {
            Authentication authentication = SecurityContextHolder.getContext().getAuthentication();
            SpringSecurityUser user = (SpringSecurityUser) authentication.getPrincipal();
            String username = user.getUsername();
            createLockJson = gitLfsObjectMapper.readValue(request.getInputStream(), GitLfsCreateLock.class);
            createLockJson.setOwner(username);
            GitLfsLock.Root res = gitLfsLocalService.createNewLock(repository.getStorage().getId(), repository.getId(), createLockJson);
            return ResponseEntity.ok(res);
        } catch (IOException e) {
            this.logger.error("Failed to parse request body into GitLfsCreateLockJson with an error: {}", e.getMessage());
            this.logger.debug("", e);
            return ResponseEntity.status(400).body("Bad Request");
        }
    }

    @ApiOperation(value = "List Locks for Verification")
    @ApiResponses(value = {@ApiResponse(code = 200, message = ""),
            @ApiResponse(code = 400, message = "An error occurred.")})
    @PreAuthorize("hasAuthority('ARTIFACTS_DEPLOY')")
    @GetMapping(value = "{storageId}/{repositoryId}/locks", produces = "application/vnd.git-lfs+json")
    public ResponseEntity<?> getLocks(@RepositoryMapping Repository repository,
                                      HttpServletRequest request,
                                      @RequestParam(name = "path", required = false) String path,
                                      @RequestParam(name = "id", required = false) String id,
                                      @RequestParam(name = "cursor", required = false) String cursor,
                                      @RequestParam(name = "limit", required = false) String limit,
                                      @RequestParam(name = "refspec", required = false) String refSpec) {
        int formattedCursor = (cursor != null) ? Integer.parseInt(cursor) : 0;
        int formattedLimit = (limit != null) ? Integer.parseInt(limit) : 0;
        return ResponseEntity.ok(gitLfsLocalService.listLocks(repository.getStorage().getId(), repository.getId(), path, id, formattedCursor, formattedLimit, refSpec));
    }


    @ApiOperation(value = "Delete Lock")
    @ApiResponses(value = {@ApiResponse(code = 200, message = ""),
            @ApiResponse(code = 400, message = "An error occurred.")})
    @PreAuthorize("hasAuthority('ARTIFACTS_DEPLOY')")
    @PostMapping(value = "{storageId}/{repositoryId}/locks/{lockId}/unlock", produces = "application/vnd.git-lfs+json", consumes = {"application/vnd.git-lfs+json", "application/json"})
    public ResponseEntity<?> deleteLock(@RepositoryMapping Repository repository,
                                        @RequestHeader HttpHeaders httpHeaders,
                                        HttpServletRequest request,
                                        @PathParam("lockId") String lockId) {
        final String storageId = repository.getStorage().getId();
        final String repositoryId = repository.getId();
        GitLfsDeleteLock deleteLockRequest;
        try {
            deleteLockRequest = gitLfsObjectMapper.readValue(request.getInputStream(), GitLfsDeleteLock.class);
        } catch (Exception e) {
            this.logger.error("Failed to parse request body into GitLfsCreateLockJson with an error: {}", e.getMessage());
            this.logger.debug("", e);
            return ResponseEntity.status(400).body("Bad Request");
        }
        return ResponseEntity.ok(gitLfsLocalService.deleteLock(storageId, repositoryId, deleteLockRequest, lockId));
    }

    public GitLfsBatchRes.LfsErrorRes checkDowloadFile(String storageId, String repositoryId, String path) {
        logger.info("checkDowloadFile /{}/{}/{}.", storageId, repositoryId, path);
        GitLfsBatchRes.LfsErrorRes errorRes = null;
        try {
            RepositoryPath repositoryPath = artifactResolutionService.resolvePath(storageId, repositoryId, path);
            if (repositoryPath == null) {
                errorRes = new GitLfsBatchRes.LfsErrorRes(404, "Object does not exist");
            }
        } catch (IOException e) {
            logger.error("Exception when getting object:{}", e.getMessage());
            errorRes = new GitLfsBatchRes.LfsErrorRes(500, "Exception when getting object");
        }
        return errorRes;
    }

    public GitLfsBatchRes setUploadRes(GitLfsBatchReq req, String storageId, String repositoryId, String auth) {
        GitLfsBatchRes res = new GitLfsBatchRes();
        List<GitLfsBatchRes.LfsObjectRes> objects = req.getObjects().stream().map(item -> {
            GitLfsBatchRes.LfsObjectRes lfsObjectRes = new GitLfsBatchRes.LfsObjectRes();
            lfsObjectRes.setOid(item.getOid());
            lfsObjectRes.setSize(item.getSize());
            GitLfsBatchRes.LfsUploadRes lfsUploadRes = new GitLfsBatchRes.LfsUploadRes();
            String baserUrl = configurationManager.getConfiguration().getBaseUrl();
            baserUrl = baserUrl.endsWith("/") ? baserUrl.substring(0, baserUrl.length() - 1) : baserUrl;
            String uploadUrl = String.format("%s/storages/%s/%s/objects/%s", baserUrl, storageId, repositoryId, getPath(item.getOid(), req.getOperation(), req.getHashAlgo()));
            lfsUploadRes.setHref(uploadUrl);
            GitLfsBatchRes.LfsHeaderRes lfsHeaderRes = new GitLfsBatchRes.LfsHeaderRes();
            lfsHeaderRes.setAuthorization(auth);
            lfsHeaderRes.setSha256(item.getOid());
            lfsUploadRes.setHeader(lfsHeaderRes);
            GitLfsBatchRes.LfsLinksRes lfsLinksRes = new GitLfsBatchRes.LfsLinksRes();
            lfsLinksRes.setUpload(lfsUploadRes);
            lfsObjectRes.setLinks(lfsLinksRes);
            return lfsObjectRes;
        }).collect(Collectors.toList());
        return res.setObjects(objects);
    }

    public GitLfsBatchRes setDownloadRes(GitLfsBatchReq req, String storageId, String repositoryId) {
        GitLfsBatchRes res = new GitLfsBatchRes();
        List<GitLfsBatchRes.LfsObjectRes> objects = req.getObjects().stream().map(item -> {
            GitLfsBatchRes.LfsObjectRes lfsObjectRes = new GitLfsBatchRes.LfsObjectRes();
            lfsObjectRes.setOid(item.getOid());
            lfsObjectRes.setSize(item.getSize());
            GitLfsBatchRes.LfsDownloadRes lfsDownloadRes = new GitLfsBatchRes.LfsDownloadRes();
            String baserUrl = configurationManager.getConfiguration().getBaseUrl();
            baserUrl = baserUrl.endsWith("/") ? baserUrl.substring(0, baserUrl.length() - 1) : baserUrl;
            String dowloadUrl = String.format("%s/storages/%s/%s/objects/%s", baserUrl, storageId, repositoryId, getPath(item.getOid(), req.getOperation(), req.getHashAlgo()));
            lfsDownloadRes.setHref(dowloadUrl);
            //GitLfsBatchRes.LfsErrorRes errorRes = checkDowloadFile(storageId,  repositoryId, getPath(item.getOid(), req.getOperation(), req.getHashAlgo()));
            //lfsDownloadRes.setError(errorRes);
            GitLfsBatchRes.LfsLinksRes lfsLinksRes = new GitLfsBatchRes.LfsLinksRes();
            lfsLinksRes.setDownload(lfsDownloadRes);
            lfsObjectRes.setLinks(lfsLinksRes);
            return lfsObjectRes;
        }).collect(Collectors.toList());
        return res.setObjects(objects);
    }

    public String getPath(String hash, String operation, String hashAlgo) {
        if (hash == null || hash.length() < 4) {
            throw new IllegalArgumentException("hash string must be at least 4 characters long");
        }
        String firstPart = hash.substring(0, 2);
        String secondPart = hash.substring(2, 4);
        return firstPart + "/" + secondPart + "/" + hash;
    }

}
