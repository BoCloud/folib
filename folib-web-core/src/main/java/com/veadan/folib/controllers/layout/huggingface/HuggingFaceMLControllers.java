package com.veadan.folib.controllers.layout.huggingface;

import com.veadan.folib.controllers.BaseArtifactController;
import com.veadan.folib.domain.gitls.model.GitLfsBatchJson;
import com.veadan.folib.domain.huggingface.constant.MlModelSystemProperties;
import com.veadan.folib.domain.huggingface.model.request.MlCommitInfo;
import com.veadan.folib.domain.huggingface.model.request.MlFilesRequest;
import com.veadan.folib.domain.huggingface.model.request.MlFilesResponse;
import com.veadan.folib.domain.huggingface.model.request.MlModelRequestContext;
import com.veadan.folib.domain.huggingface.repository.MlModelLocalRepository;
import com.veadan.folib.providers.layout.HuggingFaceLayoutProvider;
import com.veadan.folib.storage.repository.Repository;
import com.veadan.folib.web.LayoutRequestMapping;
import com.veadan.folib.web.RepositoryMapping;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import lombok.Generated;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;


import javax.inject.Inject;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.io.InputStream;
import java.util.concurrent.Semaphore;


@RestController
@LayoutRequestMapping(HuggingFaceLayoutProvider.ALIAS)
@Api(value = "Hugging Face ML 坐标控制器", tags = "Hugging Face ML 坐标控制器")
public class HuggingFaceMLControllers extends BaseArtifactController {

    private static final Logger log = LoggerFactory.getLogger(HuggingFaceMLControllers.class);
    @Inject
    private MlModelLocalRepository mlModelLocalRepository;

    private static final String EMPTY_REVISION = "tmp";

    private static final String HF_DEFAULT_REVISION = "main";

    //并发控制
    private static final int MAX_CONCURRENT_THREADS = 5;
    private final Semaphore concurrentCommitExecutionsLimit = new Semaphore(MAX_CONCURRENT_THREADS);


    @ApiOperation(value = "从具有组织名称的特定修订中获取文件头响应", nickname = "getFileHeaderWithOrganizationParam")
    @PreAuthorize("hasAuthority('ARTIFACTS_RESOLVE')")
    @RequestMapping(path = "{storageId}/{repositoryId}/{organization}/{modelName}/resolve/{revision}/{filename:.+}",
            method = RequestMethod.HEAD
    )
    public ResponseEntity<?> getHead(@RepositoryMapping Repository repository,
                                     HttpServletRequest request,
                                     HttpServletResponse response,
                                     @PathVariable("organization") String organizationName,
                                     @PathVariable("modelName") String modelName,
                                     @PathVariable("revision") String revision,
                                     @PathVariable("filename") String filename) {
        MlModelRequestContext context = MlModelRequestContext.builder()
                .storageId(repository.getStorage().getId())
                .repositoryId(repository.getId())
                .org(organizationName)
                .modelName(modelName)
                .revision(revision)
                .file(filename)
                .request(request).build();
        return mlModelLocalRepository.fetchHeaders(context,response);
    }


    @ApiOperation(value = "从没有组织名称的特定修订中获取文件头响应", nickname = "getFileHeaderWithoutOrganizationParam")
    @PreAuthorize("hasAuthority('ARTIFACTS_RESOLVE')")
    @RequestMapping(path = "{storageId}/{repositoryId}/{modelName}/resolve/{revision}/{filename:.+}",
            method = RequestMethod.HEAD
    )
    public ResponseEntity<?> getHead(@RepositoryMapping Repository repository,
                                     HttpServletRequest request,
                                     HttpServletResponse response,
                                     @PathVariable("modelName") String modelName,
                                     @PathVariable("revision") String revision,
                                     @PathVariable("filename") String filename) {
        MlModelRequestContext context = MlModelRequestContext.builder()
                .storageId(repository.getStorage().getId())
                .repositoryId(repository.getId())
                .modelName(modelName)
                .revision(revision)
                .file(filename)
                .request(request)
                .build();
        return mlModelLocalRepository.fetchHeaders(context,response);
    }


    @ApiOperation(value = "从具有组织名称的特定修订版中获取文件", nickname = "getFileWithOrganizationParam")
    @PreAuthorize("hasAuthority('ARTIFACTS_RESOLVE')")
    @RequestMapping(path = "{storageId}/{repositoryId}/{organization}/{modelName}/resolve/{revision}/{filename:.+}",
            method = RequestMethod.GET
    )
    public ResponseEntity<?> getFile(@RepositoryMapping Repository repository,
                                     HttpServletRequest request,
                                     @PathVariable("organization") String organizationName,
                                     @PathVariable("modelName") String modelName,
                                     @PathVariable("revision") String revision,
                                     @PathVariable("filename") String filename) {
        MlModelRequestContext context = MlModelRequestContext.builder()
                .storageId(repository.getStorage().getId())
                .repositoryId(repository.getId())
                .org(organizationName).modelName(modelName)
                .revision(revision)
                .file(filename)
                .request(request)
                .build();
        return mlModelLocalRepository.fetchFile(context);
    }


    @ApiOperation(value = "从没有组织名称的特定修订版本中获取文件", nickname = "getFileWithoutOrganizationParam")
    @PreAuthorize("hasAuthority('ARTIFACTS_RESOLVE')")
    @RequestMapping(path = "{storageId}/{repositoryId}/{modelName}/resolve/{revision}/{filename:.+}",
            method = RequestMethod.GET
    )
    public ResponseEntity<?> getFile(@RepositoryMapping Repository repository,
                                     HttpServletRequest request,
                                     @PathVariable("modelName") String modelName,
                                     @PathVariable("revision") String revision,
                                     @PathVariable("filename") String filename) {
        MlModelRequestContext context = MlModelRequestContext.builder()
                .storageId(repository.getStorage().getId())
                .repositoryId(repository.getId())
                .modelName(modelName)
                .revision(revision)
                .file(filename)
                .request(request)
                .build();
        return mlModelLocalRepository.fetchFile(context);
    }

    @ApiOperation(value = "获取带有组织参数的主要修订信息", nickname = "getMainRevisionInfoWithOrganizationParam")
    @PreAuthorize("hasAuthority('ARTIFACTS_RESOLVE')")
    @RequestMapping(path = "{storageId}/{repositoryId}/api/models/{organization}/{modelName}",
            method = RequestMethod.GET
    )
    public ResponseEntity<?> getMainRevisionData(@RepositoryMapping Repository repository,
                                                 HttpServletRequest request,
                                                 @PathVariable("organization") String organizationName,
                                                 @PathVariable("modelName") String modelName) {
        MlModelRequestContext context = MlModelRequestContext.builder()
                .storageId(repository.getStorage().getId())
                .repositoryId(repository.getId())
                .org(organizationName)
                .modelName(modelName)
                .revision(HF_DEFAULT_REVISION)
                .request(request).build();
        return ResponseEntity.ok(mlModelLocalRepository.fetchRevisionData(context));
    }

    @ApiOperation(value = "获取没有组织参数的主要修订信息", nickname = "getMainRevisionInfoWithoutOrganizationParam")
    @PreAuthorize("hasAuthority('ARTIFACTS_RESOLVE')")
    @RequestMapping(path = "{storageId}/{repositoryId}/api/models/{modelName}",
            method = RequestMethod.GET
    )
    public ResponseEntity<?> getMainRevisionData(@RepositoryMapping Repository repository,
                                                 HttpServletRequest request,
                                                 @PathVariable("modelName") String modelName) {
        MlModelRequestContext context = MlModelRequestContext.builder()
                .storageId(repository.getStorage().getId())
                .repositoryId(repository.getId())
                .modelName(modelName)
                .revision(HF_DEFAULT_REVISION)
                .request(request)
                .build();
        return ResponseEntity.ok(mlModelLocalRepository.fetchRevisionData(context));
    }


    @ApiOperation(value = "获取组织参数的修订信息", nickname = "getRevisionInfoWithOrganizationParam")
    @PreAuthorize("hasAuthority('ARTIFACTS_RESOLVE')")
    @RequestMapping(path = "{storageId}/{repositoryId}/api/models/{organization}/{modelName}/revision/{revision}",
            method = RequestMethod.GET
    )
    public ResponseEntity<?> getRevisionData(@RepositoryMapping Repository repository,
                                             HttpServletRequest request,
                                             @PathVariable("organization") String organizationName,
                                             @PathVariable("modelName") String modelName,
                                             @PathVariable("revision") String revision) {
        MlModelRequestContext context = MlModelRequestContext.builder()
                .storageId(repository.getStorage().getId())
                .repositoryId(repository.getId())
                .org(organizationName)
                .modelName(modelName)
                .revision(revision)
                .request(request)
                .build();
        return ResponseEntity.ok(mlModelLocalRepository.fetchRevisionData(context));
    }

    @ApiOperation(value = "获取没有组织参数的修订信息", nickname = "getRevisionInfoWithoutOrganizationParam")
    @RequestMapping(path = "{storageId}/{repositoryId}/api/models/{modelName}/revision/{revision}",
            method = RequestMethod.GET
    )
    @PreAuthorize("hasAuthority('ARTIFACTS_RESOLVE')")
    public ResponseEntity<?> getRevisionData(@RepositoryMapping Repository repository,
                                             HttpServletRequest request,
                                             @PathVariable("modelName") String modelName,
                                             @PathVariable("revision") String revision) {
        MlModelRequestContext context = MlModelRequestContext.builder()
                .storageId(repository.getStorage().getId())
                .repositoryId(repository.getId())
                .modelName(modelName)
                .revision(revision)
                .request(request)
                .build();
        return ResponseEntity.ok(mlModelLocalRepository.fetchRevisionData(context));
    }

    @ApiOperation(value = "上传组织名称参数的 LFS", nickname = "UploadLFSWithOrganizationNameParam")
    @PreAuthorize("hasAuthority('ARTIFACTS_RESOLVE')")
    @RequestMapping(path = "{storageId}/{repositoryId}/api/complete_multipart/{organization}/{modelName}/{oid}",
            method = RequestMethod.PUT
    )
    public ResponseEntity<?> uploadMultipart(@RepositoryMapping Repository repository,
                                             HttpServletRequest request,
                                             @PathVariable("organization") String organization,
                                             @PathVariable("modelName") String modelName,
                                             @PathVariable("oid") String oid, InputStream inputStream) {
        MlModelRequestContext context = MlModelRequestContext.builder()
                .storageId(repository.getStorage().getId())
                .repositoryId(repository.getId())
                .org(organization)
                .modelName(modelName)
                .file(oid)
                .request(request)
                .revision(EMPTY_REVISION)
                .build();
        return mlModelLocalRepository.uploadLfsFile(context, inputStream);
    }

    @ApiOperation(value = "上传没有组织名称参数的 LFS", nickname = "UploadLFSWithoutOrganizationNameParam")
    @PreAuthorize("hasAuthority('ARTIFACTS_RESOLVE')")
    @RequestMapping(path = "{storageId}/{repositoryId}/api/complete_multipart/{modelName}/{oid}",
            method = RequestMethod.PUT
    )
    public ResponseEntity<?> uploadMultipart(@RepositoryMapping Repository repository,
                                             HttpServletRequest request,
                                             @PathVariable("modelName") String modelName,
                                             @PathVariable("oid") String oid) throws IOException {
        MlModelRequestContext context = MlModelRequestContext.builder()
                .storageId(repository.getStorage().getId())
                .repositoryId(repository.getId())
                .modelName(modelName)
                .file(oid)
                .request(request)
                .revision(EMPTY_REVISION).build();
        return mlModelLocalRepository.uploadLfsFile(context, request.getInputStream());
    }


    @ApiOperation(value = "处理有组织名称的 LFS 对象", nickname = "PreUploadLFSWithOrganizationNameParam")
    @PreAuthorize("hasAuthority('ARTIFACTS_RESOLVE')")
    @RequestMapping(path = "{storageId}/{repositoryId}/{organization}/{modelName}.git/info/lfs/objects/batch",
            method = RequestMethod.POST,
            consumes = {"application/vnd.git-lfs+json"},
            produces = {"application/vnd.git-lfs+json"}
    )
    public ResponseEntity<?> handleLfsObjects(@RepositoryMapping Repository repository,
                                              HttpServletRequest request,
                                              @PathVariable("organization") String organization,
                                              @PathVariable("modelName") String modelName,
                                              @RequestBody GitLfsBatchJson lfsInfoPayload) {
        MlModelRequestContext context = MlModelRequestContext.builder()
                .storageId(repository.getStorage().getId())
                .repositoryId(repository.getId())
                .org(organization)
                .modelName(modelName)
                .request(request)
                .build();
        return ResponseEntity.ok(mlModelLocalRepository.handleLfsPreUpload(context,lfsInfoPayload));
    }


    @ApiOperation(value = "处理没有组织名称的 LFS 对象", nickname = "PreUploadLFSWithoutOrganizationNameParam")
    @PreAuthorize("hasAuthority('ARTIFACTS_RESOLVE')")
    @RequestMapping(path = "{storageId}/{repositoryId}/{modelName}.git/info/lfs/objects/batch",
            method = RequestMethod.POST,
            consumes = {"application/vnd.git-lfs+json"},
            produces = {"application/vnd.git-lfs+json"}
    )
    public ResponseEntity<?> handleLfsObjects(@RepositoryMapping Repository repository,
                                              HttpServletRequest request,
                                              @PathVariable("modelName") String modelName,
                                              @RequestBody GitLfsBatchJson lfsInfoPayload) {
        MlModelRequestContext context = MlModelRequestContext.builder()
                .storageId(repository.getStorage().getId())
                .repositoryId(repository.getId())
                .modelName(modelName)
                .request(request)
                .build();
        return ResponseEntity.ok(mlModelLocalRepository.handleLfsPreUpload(context,lfsInfoPayload));
    }


    @ApiOperation(value = "获取 preUpload 响应，以管理具有组织名称的每个文件的上传类型", nickname = "PreUploadModelWithOrganizationNameParam")
    @PreAuthorize("hasAuthority('ARTIFACTS_RESOLVE')")
    @RequestMapping(path = "{storageId}/{repositoryId}/api/models/{organization}/{modelName}/preupload/{revision}",
            method = RequestMethod.POST,
            consumes = {"application/json"},
            produces = {"application/json"}
    )
    public ResponseEntity<?> handlePreUpload(@RepositoryMapping Repository repository,
                                             HttpServletRequest request,
                                             @PathVariable("organization") String organizationName,
                                             @PathVariable("modelName") String modelName,
                                             @PathVariable("revision") String revision,
                                             @RequestBody MlFilesRequest filesRequest) {
        MlModelRequestContext context = MlModelRequestContext.builder()
                .storageId(repository.getStorage().getId())
                .repositoryId(repository.getId())
                .org(organizationName)
                .modelName(modelName)
                .revision(revision)
                .request(request)
                .file("")
                .build();
        MlFilesResponse mlFilesResponse = mlModelLocalRepository.handlePreUpload(context, filesRequest);
        return ResponseEntity.ok(mlFilesResponse);
    }


    @ApiOperation(value = "获取 preUpload 响应，以管理每个文件的上传类型，而无需组织名称", nickname = "PreUploadModelWithoutOrganizationNameParam")
    @PreAuthorize("hasAuthority('ARTIFACTS_RESOLVE')")
    @RequestMapping(path = "{storageId}/{repositoryId}/api/models/{modelName}/preupload/{revision}",
            method = RequestMethod.POST,
            consumes = {"application/json"},
            produces = {"application/json"}
    )
    public ResponseEntity<?> handlePreUpload(@RepositoryMapping Repository repository,
                                             HttpServletRequest request,
                                             @PathVariable("modelName") String modelName,
                                             @PathVariable("revision") String revision,
                                             @RequestBody MlFilesRequest filesRequest) {
        MlModelRequestContext context = MlModelRequestContext.builder()
                .storageId(repository.getStorage().getId())
                .repositoryId(repository.getId())
                .modelName(modelName)
                .revision(revision)
                .request(request)
                .file("")
                .build();
        MlFilesResponse mlFilesResponse = mlModelLocalRepository.handlePreUpload(context, filesRequest);
        return ResponseEntity.ok(mlFilesResponse);
    }


    @ApiOperation(value = "将模型上传为具有组织名称的新修订版本", nickname = "UploadModelWithOrganizationNameParam")
    @PreAuthorize("hasAuthority('ARTIFACTS_RESOLVE')")
    @RequestMapping(path = "{storageId}/{repositoryId}/api/models/{organization}/{modelName}/commit/{revision}",
            method = RequestMethod.POST,
            consumes = {"application/x-ndjson"},
            produces = {"application/json"}
    )
    public ResponseEntity<?> handleCommit(@RepositoryMapping Repository repository,
                                          HttpServletRequest request,
                                          @PathVariable("organization") String organization,
                                          @PathVariable("modelName") String modelName,
                                          @PathVariable("revision") String revision,
                                          InputStream bodyStream) {
        MlModelRequestContext context = MlModelRequestContext.builder()
                .storageId(repository.getStorage().getId())
                .repositoryId(repository.getId())
                .org(organization)
                .modelName(modelName)
                .revision(revision)
                .request(request)
                .build();
        if (!this.concurrentCommitExecutionsLimit.tryAcquire()) {
            log.info("Upload for organization {}, modelName {} and revision {} is cancelled. Concurrent uploads limit {} reached.", organization, modelName, revision, MlModelSystemProperties.ML_MODEL_CONCURRENT_UPLOADS.name());
            return ResponseEntity.status(HttpStatus.TOO_MANY_REQUESTS).build();
        }
        try {
            log.debug("Handle commit payload size is {} bytes", request.getHeader("content-length"));
            String commitMessage = mlModelLocalRepository.handleCommit(context, bodyStream);
            return ResponseEntity.ok(new MlCommitInfo("commitUrl", commitMessage, commitMessage, "commitOid", null));
        } finally {
            this.concurrentCommitExecutionsLimit.release();
        }
    }


    @ApiOperation(value = "将模型作为新修订版本上传，不带组织名称", nickname = "UploadModelWithoutOrganizationNameParam")
    @PreAuthorize("hasAuthority('ARTIFACTS_RESOLVE')")
    @RequestMapping(path = "{storageId}/{repositoryId}/api/models/{modelName}/commit/{revision}",
            method = RequestMethod.POST,
            consumes = {"application/x-ndjson"},
            produces = {"application/json"}
    )
    public ResponseEntity<?> handleCommit(@RepositoryMapping Repository repository,
                                          HttpServletRequest request,
                                          @PathVariable("modelName") String modelName,
                                          @PathVariable("revision") String revision,
                                          InputStream body) {
        MlModelRequestContext context = MlModelRequestContext.builder()
                .storageId(repository.getStorage().getId())
                .repositoryId(repository.getId())
                .modelName(modelName)
                .revision(revision)
                .request(request)
                .build();
        String commitMessage = mlModelLocalRepository.handleCommit(context, body);
        return ResponseEntity.ok(new MlCommitInfo("commitUrl", commitMessage, commitMessage, "commitOid", null));
    }

    @ApiOperation(value = "自动通过元数据验证", nickname = "AutoPassMetadataValidation")
    @PreAuthorize("hasAuthority('ARTIFACTS_RESOLVE')")
    @RequestMapping(path = "{storageId}/{repositoryId}/api/validate-yaml",
            method = RequestMethod.POST,
            consumes = {"application/json"},
            produces = {"application/json"}
    )
    public ResponseEntity<?> passMetadataValidation(InputStream body) {
        return ResponseEntity.ok().build();
    }

}
