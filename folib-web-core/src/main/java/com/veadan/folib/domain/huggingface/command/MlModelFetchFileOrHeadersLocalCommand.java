package com.veadan.folib.domain.huggingface.command;

import javax.ws.rs.core.Response;

import com.veadan.folib.domain.huggingface.model.RevisionData;
import com.veadan.folib.domain.huggingface.model.request.MlModelRequestContext;
import com.veadan.folib.domain.huggingface.utils.MlModelUtils;
import lombok.Generated;
import lombok.NonNull;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.ResponseEntity;

/**
 * 模型获取文件或标头本地
 */
public class MlModelFetchFileOrHeadersLocalCommand {
    @Generated
    private static final Logger log = LoggerFactory.getLogger(MlModelFetchFileOrHeadersLocalCommand.class);

    public MlModelFetchFileOrHeadersLocalCommand() {
    }

    public ResponseEntity<?> fetchFile(MlModelRequestContext requestContext, RevisionData modelInfo) {
        if (requestContext == null) {
            throw new NullPointerException("requestContext is marked non-null but is null");
        }
        if (modelInfo == null) {
            throw new NullPointerException("modelInfo is marked non-null but is null");
        }
        return fetch(requestContext, true, modelInfo);
    }

    public ResponseEntity<?> fetchHeaders(MlModelRequestContext requestContext, RevisionData modelInfo) {
        if (requestContext == null) {
            throw new NullPointerException("requestContext is marked non-null but is null");
        }
        if (modelInfo == null) {
            throw new NullPointerException("modelInfo is marked non-null but is null");
        }
        return fetch(requestContext, false, modelInfo);
    }

    private ResponseEntity<?> fetch(MlModelRequestContext requestContext, boolean isFile, RevisionData modelInfo) {
        int invalidStatus;
        if (requestContext == null) {
            throw new NullPointerException("requestContext is marked non-null but is null");
        }
        if (modelInfo == null) {
            throw new NullPointerException("modelInfo is marked non-null but is null");
        }
        String repoKey = requestContext.getRepositoryId();
        String organization = requestContext.getOrg();
        String modelName = requestContext.getModelName();
        String filename = requestContext.getFile();
        String revisionFolder = requestContext.getVersionFolder();
        log.debug("Received fetch {} request for repo {}, organization {}, model {}, generatedSha1 {}, fileName {}", isFile ? "file" : "header", repoKey, organization, modelName, modelInfo.getSha(), filename);
        String artifactPath = MlModelUtils.getFilePath(organization, modelName, revisionFolder, modelInfo
                .getLastModified(), filename);
        return ResponseEntity.ok().build();
        // TODO: 2024/6/6
        //PackageDownloadContext context = new PackageDownloadContext(repoKey, artifactPath);
        //context.setSkipPackageUsageTrack(!isFile);
        //context.enableRedirect();
        //try {
        //    PackageArtifact artifact = this.downloadService.getArtifact(context);
        //    Response response = this.downloadService.download(context);
        //    if (HttpUtils.isRedirectionResponseCode(response.getStatus())) {
        //        log.debug("Got redirect response for download artifact, repo:{}, path:{}", context
        //                .getRepoKey(), context.getPath());
        //        return buildRedirectResponse(response, artifact.getLength(), artifact.getSha2(), modelInfo.getSha());
        //    }
        //    if (HttpUtils.isSuccessfulResponseCode(response.getStatus())) {
        //        log.debug("Got successful response for download artifact, repo:{}, path:{}", context
        //                .getRepoKey(), context.getPath());
        //        return buildSuccessfulResponse(response, artifact.getLength(), artifact.getSha2(), modelInfo.getSha());
        //    }
        //    invalidStatus = response.getStatus();
        //} catch (PackageNotFoundException e) {
        //    log.warn("Failed to find artifact {} in repo {}", artifactPath, repoKey);
        //    return returnErrorResponse();
        //} catch (PackageException e) {
        //    log.warn("Failed to fetch {} for repo {}, organization {}, model {}, revision {}, fileName {}", new Object[]{isFile ? "file" : "header", repoKey, organization, modelName, modelInfo.getSha(), filename});
        //    throw e;
        //}
        //log.error("Did not get redirect status response or 200 ok for: repo:{}, path:{} status:{}", new Object[]{context
        //        .getRepoKey(), context.getPath(), Integer.valueOf(invalidStatus)});
        //throw new RuntimeException("Could not fetch artifact");
    }

    private static Response returnErrorResponse() {
        return Response.status(Response.Status.NOT_FOUND).header("X-Error-Code", "EntryNotFound")
                .header("X-Error-Message", "EntryNotFound").build();
    }

    private static Response buildRedirectResponse(Response response, long size, String etag, String repoCommit) {
        if (response == null) {
            throw new NullPointerException("response is marked non-null but is null");
        }
        if (etag == null) {
            throw new NullPointerException("etag is marked non-null but is null");
        }
        if (repoCommit == null) {
            throw new NullPointerException("repoCommit is marked non-null but is null");
        }
        return Response.status(response.getStatus())
                .entity(response.getEntity())
                .header("X-Linked-ETag", etag)
                .header("X-Linked-Size", Long.valueOf(size))
                .header("X-Repo-Commit", repoCommit)
                .header("Location", response.getHeaderString("Location")).build();
    }

    private static Response buildSuccessfulResponse(Response response, long size, String etag, String repoCommit) {
        if (response == null) {
            throw new NullPointerException("response is marked non-null but is null");
        }
        if (etag == null) {
            throw new NullPointerException("etag is marked non-null but is null");
        }
        if (repoCommit == null) {
            throw new NullPointerException("repoCommit is marked non-null but is null");
        }
        return Response.ok().entity(response.getEntity())
                .header("ETag", etag)
                .header("Content-Length", Long.valueOf(size))
                .header("X-Repo-Commit", repoCommit).build();
    }
}

