package com.veadan.folib.controllers.layout.gitlfs;

import com.veadan.folib.controllers.BaseArtifactController;
import com.veadan.folib.model.request.GitLfsBatchReq;
import com.veadan.folib.model.response.GitLfsBatchRes;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.providers.layout.GitLfsLayoutProvider;
import com.veadan.folib.storage.repository.Repository;
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
import org.springframework.web.bind.annotation.*;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.util.List;
import java.util.stream.Collectors;


@RestController
@LayoutRequestMapping(GitLfsLayoutProvider.ALIAS)
@Api(value = "gitlfs坐标控制器", tags = "gitlfs坐标控制器")
public class GitLfsArtifactController extends BaseArtifactController {

    private static final String UPLOAD = "upload";
    private static final String DOWNLOAD = "download";

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
    @PutMapping(value = "{storageId}/{repositoryId}/{path:.+}", consumes ="application/*")
    public ResponseEntity<?> upload(@RepositoryMapping Repository repository,
                                 @PathVariable String path,
                                 HttpServletRequest request) {
        final String storageId = repository.getStorage().getId();
        final String repositoryId = repository.getId();

        try {
            RepositoryPath repositoryPath = repositoryPathResolver.resolve(storageId, repositoryId, path);
            artifactManagementService.validateAndStore(repositoryPath, request.getInputStream());

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
    @PreAuthorize("hasAuthority('ARTIFACTS_RESOLVE')")
    @PostMapping(value = {"{storageId}/{repositoryId}/locks/verify"})
    public ResponseEntity<?> verify(@RepositoryMapping Repository repository,
                         @RequestHeader HttpHeaders httpHeaders,
                         HttpServletRequest request,
                         HttpServletResponse response){

        return  ResponseEntity.status(HttpStatus.NOT_FOUND).build();

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
            String uploadUrl = String.format("%s/storages/%s/%s/objects/%s",baserUrl,storageId,repositoryId,getPath(item.getOid(), req.getOperation(), req.getHashAlgo()));
            lfsUploadRes.setHref(uploadUrl );
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
            String dowloadUrl = String.format("%s/storages/%s/%s/objects/%s",baserUrl,storageId,repositoryId,getPath(item.getOid(), req.getOperation(), req.getHashAlgo()));
            lfsDownloadRes.setHref(dowloadUrl);
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
        return firstPart + "/" + secondPart + "/" + hash;//UPLOAD.equals(operation) ? firstPart + "/" + secondPart + "/" + hash  : firstPart + "/" + secondPart + "/" + hash;
    }

}
