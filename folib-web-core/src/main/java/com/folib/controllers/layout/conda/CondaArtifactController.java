package com.folib.controllers.layout.conda;

import cn.hutool.core.io.FileUtil;
import com.folib.artifact.coordinates.CondaCoordinates;
import com.folib.controllers.BaseArtifactController;
import com.folib.event.CondaRepodataEvent;
import com.folib.index.cache.CondaIndexCache;
import com.folib.index.model.Index;
import com.folib.index.model.RepoDataEventKind;
import com.folib.index.model.RepoDataPackage;
import com.folib.providers.io.RepositoryPath;
import com.folib.providers.io.RepositoryPathResolver;
import com.folib.services.ArtifactManagementService;
import com.folib.services.ArtifactResolutionService;
import com.folib.services.CondaArtifactService;
import com.folib.services.CondaRepoDataService;
import com.folib.web.LayoutReqMapping;
import com.folib.web.RepoMapping;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;
import jakarta.inject.Inject;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Lazy;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.ui.ModelMap;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.multipart.MultipartFile;
import com.folib.storage.repository.Repository;

import java.io.File;
import java.io.FileInputStream;
import java.io.InputStream;
import java.nio.file.Path;
import java.util.Date;
import java.util.Objects;
import java.util.Optional;
import java.util.UUID;

/**
 * @author LingengMa
 * @date 2025/04/07 09:12
 */
@RestController
@LayoutReqMapping(CondaCoordinates.LAYOUT_NAME)
@Slf4j
@Api(description = "conda坐标控制器", tags = "conda坐标控制器")
public class CondaArtifactController extends BaseArtifactController {

    @Lazy
    @Inject
    private CondaRepoDataService condaRepoDataService;
    @Lazy
    @Inject
    private CondaArtifactService condaArtifactService;
    @Lazy
    @Inject
    private ArtifactManagementService artifactManagementService;
    @Lazy
    @Inject
    private RepositoryPathResolver repositoryPathResolver;
    @Lazy
    @Inject
    protected ArtifactResolutionService artifactResolutionService;

    @Value("${folib.temp}")
    private String tempPath;
    @Lazy
    @Inject
    private CondaIndexCache condaIndexCache;

    @Override
    @PreAuthorize("authenticated")
    @GetMapping(value = "/{storageId}/{repositoryId}/")
    public ResponseEntity<String> checkRepositoryAccess() {
        return super.checkRepositoryAccess();
    }


    @ApiOperation(value = "Upload conda artifact")
    @ApiResponses(value = {
            @ApiResponse(code = 200, message = "Artifact uploaded successfully"),
            @ApiResponse(code = 500, message = "Internal server error")
    })
    @PreAuthorize("hasAuthority('ARTIFACTS_DEPLOY')")
    @RequestMapping(value = "/{storageId}/{repositoryId}",
            consumes = MediaType.MULTIPART_FORM_DATA_VALUE,
            method = {RequestMethod.POST, RequestMethod.PUT})
    public ResponseEntity<?> uploadArtifact(@RepoMapping Repository repository,
                                            @RequestParam("package") MultipartFile file) {
        if (file.isEmpty()) {
            return ResponseEntity.badRequest().body("File is empty");
        }
        String fileName = file.getOriginalFilename();
        // 新增文件名校验
        if (fileName == null || fileName.trim().isEmpty()) {
            return ResponseEntity.badRequest().body("Invalid file name");
        }
        if (!fileName.endsWith(".conda") && !fileName.endsWith(".tar.bz2")) {
            return ResponseEntity.badRequest().body("Please upload a .conda or .tar.bz2 file");
        }

        File parentTempFile = null;
        RepositoryPath artifactPath = null;
        String storageId = repository.getStorage().getId();
        String repositoryId = repository.getId();

        try {
            // 1. 存储到临时目录
            parentTempFile = new File(tempPath + File.separator + UUID.randomUUID() + File.separator);
            File artifactTempFile = new File(parentTempFile.getAbsolutePath() + File.separator + fileName);
            Path artifactTempPath = Path.of(artifactTempFile.getAbsolutePath());
            InputStream is = file.getInputStream();
            FileUtil.writeFromStream(is, artifactTempFile);

            // 2. 提取元数据
            Index index = condaArtifactService.extract(artifactTempPath.getParent().toString(), fileName);
            if (index == null) {
                throw new RuntimeException("Failed to extract metadata");
            }

            // 3. 获取platformId
            String platformId = Optional.ofNullable(index.getSubdir()).orElse("noarch");

            // 4. 构建conda路径
            artifactPath = repositoryPathResolver.resolve(storageId, repositoryId,
                    platformId + "/" + fileName);
            condaRepoDataService.getRepoData(artifactPath.getRepository(), platformId);

            // 5. 存储文件
            try (InputStream artifactInputStream = new FileInputStream(artifactTempFile)) {
                artifactManagementService.validateAndStore(artifactPath, artifactInputStream);
            } catch (Exception e) {
                log.error("store artifact：{}，error：{}", artifactPath.toAbsolutePath(), ExceptionUtils.getStackTrace(e));
                throw new RuntimeException(e.getMessage());
            }

            // 6. 更新索引
//            RepoDataPackage repoDataPackage = condaArtifactService.getRepoDataPackage(artifactPath);
            // index复用, 可减少一次io
            RepoDataPackage repoDataPackage = condaArtifactService.convertIndexToRepoDataPackage(artifactPath, index);
            CondaRepodataEvent event = new CondaRepodataEvent(RepoDataEventKind.ADD, artifactPath.getRepository(), platformId,
                    fileName,
                    repoDataPackage);
            condaRepoDataService.sendRepoDataEvent(event);
            return ResponseEntity.ok("Artifact uploaded successfully");
        } catch (Exception e) {
            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).body("Processing error: " + e.getMessage());
        } finally {
            // 9. 安全删除临时文件
            if (Objects.nonNull(parentTempFile)) {
                FileUtil.del(parentTempFile);
            }
        }
    }

    @ApiOperation(value = "Get repodata")
    @ApiResponses(value = {
            @ApiResponse(code = 200, message = "Get repodata successfully"),
            @ApiResponse(code = 404, message = "Repodata not found"),
            @ApiResponse(code = 500, message = "Internal server error")
    })
    @PreAuthorize("hasAuthority('ARTIFACTS_RESOLVE')")
    @GetMapping(value = {"/{storageId}/{repositoryId}/{platformId}/repodata.json",
            "/{storageId}/{repositoryId}/{platformId}/current_repodata.json"},
            produces = MediaType.APPLICATION_JSON_VALUE)
    public void getRepoData(@RepoMapping Repository repository,
                            @PathVariable String platformId,
                            @RequestHeader HttpHeaders httpHeaders,
                            @RequestHeader(value = "If-Modified-Since", required = false) Date ifModifiedSince,
                            HttpServletRequest httpRequest,
                            HttpServletResponse response)
            throws Exception {
        String uri = httpRequest.getRequestURI();
        String targetName = uri.substring(uri.lastIndexOf('/') + 1);
        String storageId = repository.getStorage().getId();
        String repositoryId = repository.getId();

        // 1. 构造repoData路径
        try {
            condaRepoDataService.getRepoData(repository, platformId);
        } catch (Exception e) {
            log.error("Error getting repo data", e);
            response.setStatus(HttpServletResponse.SC_NOT_FOUND);
            return;
        }

        RepositoryPath repoDataPath = artifactResolutionService.resolvePath(storageId, repositoryId, platformId + "/" + targetName);
        if (repoDataPath == null) {
            response.setStatus(HttpServletResponse.SC_NOT_FOUND);
            return;
        }

        // 2. NotModified检查
        if (condaIndexCache.isNotModified(repoDataPath.toString(), ifModifiedSince)) {
            response.setStatus(HttpServletResponse.SC_NOT_MODIFIED);
            response.setDateHeader("Last-Modified", ifModifiedSince.getTime());
            return;
        }

        provideArtifactDownloadResponse(httpRequest, response, httpHeaders, repoDataPath);
        if (!condaIndexCache.containsKey(repoDataPath.toString())) {
            condaIndexCache.put(repoDataPath.toString());
        }

    }


    @ApiOperation(value = "Get conda package")
    @ApiResponses(value = {
            @ApiResponse(code = 200, message = "Get conda package successfully"),
            @ApiResponse(code = 404, message = "Conda package not found"),
            @ApiResponse(code = 500, message = "Internal server error")
    })
    @PreAuthorize("hasAuthority('ARTIFACTS_RESOLVE')")
    @GetMapping(value = "/{storageId}/{repositoryId}/{platformId}/{packageName}",
            produces = MediaType.APPLICATION_OCTET_STREAM_VALUE)
    public void getCondaPackage(@RepoMapping Repository repository,
                                @PathVariable String platformId,
                                @PathVariable String packageName,
                                @RequestHeader HttpHeaders httpHeaders,
                                HttpServletRequest request,
                                HttpServletResponse response,
                                ModelMap model)
            throws Exception {
        download(repository, httpHeaders, platformId + "/" + packageName, request, response, model);
    }


    @ApiOperation(value = "Delete conda package")
    @ApiResponses(value = {
            @ApiResponse(code = 200, message = "DELETE conda package successfully"),
            @ApiResponse(code = 404, message = "Conda package not found"),
            @ApiResponse(code = 500, message = "Internal server error")
    })
    @PreAuthorize("hasAuthority('ARTIFACTS_DELETE')")
    @DeleteMapping(value = {"/{storageId}/{repositoryId}/{platformId}/{packageName}",
            "/{storageId}/{repositoryId}/package/{platformId}/{packageName}"})
    public ResponseEntity<?> deleteCondaPackage(@RepoMapping Repository repository,
                                                @PathVariable String platformId,
                                                @PathVariable String packageName) {
        // 1. 获取conda包路径
        RepositoryPath artifactPath = repositoryPathResolver.resolve(repository, platformId + "/" + packageName);
        try {
            condaArtifactService.unpublishPackage(artifactPath);
        } catch (Exception e) {
            log.error("Error deleting conda package", e);
            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).body("Failed to delete conda package");
        }
        // 2. 返回成功响应
        return ResponseEntity.ok("Conda package deleted successfully");
    }
}
