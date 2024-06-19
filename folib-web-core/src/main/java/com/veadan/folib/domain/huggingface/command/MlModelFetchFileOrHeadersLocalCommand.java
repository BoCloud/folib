package com.veadan.folib.domain.huggingface.command;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.ws.rs.core.Response;

import com.veadan.folib.components.artifact.ArtifactComponent;
import com.veadan.folib.domain.CacheSettings;
import com.veadan.folib.domain.huggingface.model.RevisionData;
import com.veadan.folib.domain.huggingface.model.request.MlModelRequestContext;
import com.veadan.folib.domain.huggingface.utils.HttpUtils;
import com.veadan.folib.domain.huggingface.utils.MlModelUtils;
import com.veadan.folib.exception.ExceptionHandlingOutputStream;
import com.veadan.folib.io.LazyInputStream;
import com.veadan.folib.providers.io.RepositoryFiles;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.providers.io.RepositoryPathResolver;
import com.veadan.folib.providers.layout.DockerLayoutProvider;
import com.veadan.folib.services.ArtifactResolutionService;
import com.veadan.folib.storage.metadata.MetadataHelper;
import com.veadan.folib.utils.ArtifactControllerHelper;
import lombok.Generated;
import lombok.NonNull;
import org.apache.commons.io.FilenameUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.io.FileSystemResource;
import org.springframework.core.io.InputStreamResource;
import org.springframework.core.io.Resource;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.servlet.mvc.method.annotation.StreamingResponseBody;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.nio.ByteBuffer;
import java.nio.channels.Channels;
import java.nio.channels.FileChannel;
import java.nio.channels.ReadableByteChannel;
import java.nio.channels.WritableByteChannel;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Objects;

/**
 * 模型获取文件或标头本地
 */
public class MlModelFetchFileOrHeadersLocalCommand {

    private static final Logger log = LoggerFactory.getLogger(MlModelFetchFileOrHeadersLocalCommand.class);

    private ArtifactComponent artifactComponent;

    protected ArtifactResolutionService artifactResolutionService;

    protected RepositoryPathResolver repositoryPathResolver;
    public MlModelFetchFileOrHeadersLocalCommand(ArtifactResolutionService artifactResolutionService,RepositoryPathResolver repositoryPathResolver) {
        this.artifactResolutionService = artifactResolutionService;
        this.repositoryPathResolver = repositoryPathResolver;
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
        String repositoryId = requestContext.getRepositoryId();
        String organization = requestContext.getOrg();
        String modelName = requestContext.getModelName();
        String filename = requestContext.getFile();
        String revisionFolder = requestContext.getVersionFolder();
        log.debug("Received fetch {} request for repo {}, organization {}, model {}, generatedSha1 {}, fileName {}", isFile ? "file" : "header", repositoryId, organization, modelName, modelInfo.getSha(), filename);
        String artifactPath = MlModelUtils.getFilePath(organization, modelName, revisionFolder, modelInfo
                .getLastModified(), filename);
        String artifactSh2Path = MlModelUtils.getFilePath(organization, modelName, revisionFolder, modelInfo
                .getLastModified(), String.join(".",filename,"sha256"));
        try {
            RepositoryPath repositoryPath = repositoryPathResolver.resolve(requestContext.getStorageId(),
                    requestContext.getRepositoryId(),
                    artifactPath);
            String sha2 = getArtifactsh2(requestContext.getStorageId(),requestContext.getRepositoryId(),artifactSh2Path);
            long artifactSize = getArtifactSize(requestContext.getStorageId(),requestContext.getRepositoryId(),artifactPath);
            return buildSuccessfulResponse(repositoryPath,artifactSize,sha2, modelInfo.getSha());
        } catch (Exception e) {
            log.error("Failed to find artifact {} in repo {}", artifactPath, repositoryId);
            return returnErrorResponse();
        }
    }

    protected String getArtifactsh2(String storageId, String repositoryId, String path){
        RepositoryPath sha2Path = repositoryPathResolver.resolve(storageId, repositoryId, path);
        try {
            return Files.readString(sha2Path);
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    protected long getArtifactSize(String storageId, String repositoryId, String path){
        RepositoryPath artifactPath = repositoryPathResolver.resolve(storageId, repositoryId, path);
        try {
           return Files.size(artifactPath);
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }
    private static ResponseEntity<?> returnErrorResponse() {
        return ResponseEntity.status(HttpStatus.NOT_FOUND)
                .header("X-Error-Code", "EntryNotFound")
                .header("X-Error-Message", "EntryNotFound").build();
    }


    private static ResponseEntity<?> buildSuccessfulResponse(RepositoryPath filePath, long size, String etag, String repoCommit) {
        if (etag == null) {
            throw new NullPointerException("etag is marked non-null but is null");
        }
        if (repoCommit == null) {
            throw new NullPointerException("repoCommit is marked non-null but is null");
        }
        try (InputStream in = Files.newInputStream(filePath)) {
            InputStreamResource resource = new InputStreamResource(in);
            return ResponseEntity.ok()
                    .header("ETag", etag)
                    .header("Content-Length", Long.valueOf(size).toString())
                    .header("X-Repo-Commit", repoCommit)
                    .header("Content-Type", "application/octet-stream")
                    .body(resource);
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    void artifactDownloadResponse(HttpServletResponse response,
                                  HttpHeaders httpHeaders,
                                  RepositoryPath repositoryPath) throws Exception {

        // If the response is already committed, there's no need to proceed.
        ResponseEntity entity = ResponseEntity.ok().build();
        Path path = getCachePath(repositoryPath);
        ArtifactControllerHelper.provideArtifactHeaders(response, path);
        // If the resource is not found, return false.
        long startTime = System.currentTimeMillis();
        log.debug("Download [{}] 开始时间 [{}]", repositoryPath.toString(), startTime);
        if (ArtifactControllerHelper.isRangedRequest(httpHeaders)) {
            //分片
            log.debug("RepositoryPath [{}] Detected ranged request.", path.toString());
            try (InputStream is = Files.newInputStream(path)) {
                ArtifactControllerHelper.handlePartialDownload(is, httpHeaders, response);
            }
        } else if (path.toString().startsWith("s3://")) {
            //S3
            if (path instanceof RepositoryPath) {
                try (InputStream is = artifactResolutionService.getInputStream((RepositoryPath) path)) {
                    copyToResponse(is, response);
                }
            }
        } else {
            try (FileChannel fileChannel = FileChannel.open(path);
                 WritableByteChannel responseChannel = Channels.newChannel(response.getOutputStream())) {
                long fileSize = fileChannel.size();
                for (long left = fileSize; left > 0; ) {
                    log.debug("RepositoryPath [{}] position [{}] left [{}]", path.toString(), fileSize - left, left);
                    left -= fileChannel.transferTo((fileSize - left), left, responseChannel);
                }
            }
        }
        artifactComponent.afterRead(repositoryPath);
        log.debug("Download [{}] 结束时间 [{}]", repositoryPath.toString(), System.currentTimeMillis() - startTime);

    }

    public static void copyToResponse(InputStream is,
                                      HttpServletResponse response) throws IOException {
        try (OutputStream os = new ExceptionHandlingOutputStream(response.getOutputStream());
                WritableByteChannel outputChannel = Channels.newChannel(os)) {
            ReadableByteChannel inputChannel = Channels.newChannel(is);
            ByteBuffer buffer = ByteBuffer.allocate(8192);
            while (inputChannel.read(buffer) != -1) {
                buffer.flip();
                outputChannel.write(buffer);
                buffer.clear();
            }
            response.flushBuffer();
        }
    }

    private Path getCachePath(RepositoryPath repositoryPath) {
        if (Objects.isNull(repositoryPath)) {
            return null;
        }
        Path path = repositoryPath;
        String storageId = repositoryPath.getStorageId(), repositoryId = repositoryPath.getRepositoryId();
        try {
            CacheSettings cacheSettings = artifactComponent.getCacheConfig();
            if (Objects.isNull(cacheSettings) || !cacheSettings.isEnabled()) {
                return path;
            }
            Path cacheParentPath = Files.createDirectories(Paths.get(cacheSettings.getDirectoryPath()));
            String sourcePath = repositoryPath.toString();
            String prefix = String.format("/%s/%s/", storageId, repositoryId);
            String targetSubPath = sourcePath.substring(sourcePath.indexOf(prefix) + 1);
            Path targetPath = cacheParentPath.resolve(targetSubPath);
            boolean existsCache = Files.exists(targetPath) && (RepositoryFiles.isArtifactChecksum(FilenameUtils.getName(targetPath.getFileName().toString())) || RepositoryFiles.validateChecksum(repositoryPath, targetPath) || DockerLayoutProvider.ALIAS.equals(repositoryPath.getRepository().getLayout()));
            if (existsCache) {
                log.info("存在缓存 storageId [{}] repositoryId [{}]，源制品地址 [{}] 缓存制品地址 [{}]", storageId, repositoryId, sourcePath, targetPath.toString());
                path = targetPath;
//                artifactComponent.asyncHandlerArtifactCacheRecord(repositoryPath, cacheSettings, targetPath);
            } else {
                //不存在缓存，触发缓存事件
                if (repositoryPath.toString().contains(MetadataHelper.MAVEN_METADATA_XML)) {
                    return path;
                }
                artifactComponent.artifactCache(repositoryPath);
            }
        } catch (Exception ex) {
            log.warn("缓存制品 [{}] [{}] [{}] 错误 [{}]", storageId, repositoryId, repositoryPath.toString(), ExceptionUtils.getStackTrace(ex));
        }
        return path;
    }
}

