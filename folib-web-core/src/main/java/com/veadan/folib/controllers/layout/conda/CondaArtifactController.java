package com.veadan.folib.controllers.layout.conda;

import cn.hutool.core.io.FileUtil;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.veadan.folib.artifact.coordinates.CondaArtifactCoordinates;
import com.veadan.folib.controllers.BaseArtifactController;
import com.veadan.folib.index.indexer.CondaMetadataExtractor;
import com.veadan.folib.index.model.Index;
import com.veadan.folib.index.model.RepoData;
import com.veadan.folib.index.model.RepoDataEventKind;
import com.veadan.folib.providers.ProviderImplementationException;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.providers.io.RepositoryPathResolver;
import com.veadan.folib.services.ArtifactManagementService;
import com.veadan.folib.services.CondaArtifactService;
import com.veadan.folib.services.CondaRepoDataService;
import com.veadan.folib.storage.repository.Repository;
import com.veadan.folib.storage.validation.artifact.ArtifactCoordinatesValidationException;
import com.veadan.folib.web.LayoutRequestMapping;

import com.veadan.folib.web.RepositoryMapping;
import io.github.bucket4j.distributed.remote.Request;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;


import lombok.NonNull;
import lombok.extern.slf4j.Slf4j;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.*;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.security.core.Authentication;
import org.springframework.web.multipart.MultipartFile;


import javax.inject.Inject;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.*;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
import java.util.Objects;
import java.util.Optional;
import java.util.UUID;


/**
 * @author LingengMa
 * @date 2025/04/07 09:12
 */
@RestController
@LayoutRequestMapping(CondaArtifactCoordinates.LAYOUT_NAME)
@Slf4j
@Api(description = "conda坐标控制器", tags = "conda坐标控制器")
public class CondaArtifactController extends BaseArtifactController {

    @Inject
    private CondaRepoDataService condaRepoDataService;

    @Inject
    private CondaArtifactService condaArtifactService;

    @Inject
    private CondaMetadataExtractor condaMetadataExtractor;

    @Inject
    private ArtifactManagementService artifactManagementService;

    @Inject
    private RepositoryPathResolver repositoryPathResolver;

    @Value("${folib.temp}")
    private String tempPath;


    @ApiOperation(value = "Upload conda artifact")
    @ApiResponses(value = {
            @ApiResponse(code = 200, message = "Artifact uploaded successfully"),
            @ApiResponse(code = 500, message = "Internal server error")
    })
    @PreAuthorize("hasAuthority('ARTIFACTS_DEPLOY')")
    @RequestMapping(value = "/{storageId}/{repositoryId}/conda",
            consumes = MediaType.MULTIPART_FORM_DATA_VALUE,
            method = {RequestMethod.POST, RequestMethod.PUT})
    public ResponseEntity<?> uploadArtifact(@RepositoryMapping Repository repository,
                                            @RequestParam("package") MultipartFile file) {
        if (file.isEmpty()) {
            return ResponseEntity.badRequest().body("File is empty");
        }

        String fileName = file.getOriginalFilename();
        // 新增文件名校验
        if (fileName == null || fileName.trim().isEmpty()) {
            return ResponseEntity.badRequest().body("Invalid file name");
        }

        File parentTempFile = null;
        try {
            // 1. 存储到临时目录
            parentTempFile = new File(tempPath + File.separator + UUID.randomUUID() + File.separator);
            File artifactTempFile = new File(parentTempFile.getAbsolutePath() + File.separator + fileName);
            Path artifactTempPath = Path.of(artifactTempFile.getAbsolutePath());
            InputStream is = file.getInputStream();
            FileUtil.writeFromStream(is, artifactTempFile);

            // 2. 提取元数据
            Index index = condaMetadataExtractor.extract(artifactTempPath.getParent().toString(), fileName);
            if (index == null) {
                throw new RuntimeException("Failed to extract metadata");
            }

            // 3. 获取platform
            String platform = Optional.ofNullable(index.getSubdir()).orElse("noarch");

            // 4. 构建conda路径
            RepositoryPath artifactPath = repositoryPathResolver.resolve(repository, platform + "/" + fileName);

            // 5. 检查文件是否存在
            if (condaArtifactService.checkArtifactExist(artifactPath)) {
                return ResponseEntity.status(HttpStatus.CONFLICT).body("Artifact already exists");
            }

            // 6. 存储文件
            CondaArtifactCoordinates coordinates = CondaArtifactCoordinates.of(platform, fileName);
            storeCondaPackage(repository, coordinates, artifactTempFile);

            // 7. 更新索引
            condaRepoDataService.sendRepoDataEvent(RepoDataEventKind.ADD, artifactPath.getParent().toString(), fileName);

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
    @GetMapping(value = {"/{storageId}/{repositoryId}/{platformId}/repodata.json",
                         "/{storageId}/{repositoryId}/{platformId}/current_repodata.json"},
                produces = MediaType.APPLICATION_JSON_VALUE)
    public void getRepoData(@RepositoryMapping Repository repository,
                                         @PathVariable String platformId,
                                         HttpServletRequest httpRequest,
                                         HttpServletResponse response) {
        String uri = httpRequest.getRequestURI();
        String targetName = uri.substring(uri.lastIndexOf('/') + 1);

        // 1. 获取RepoData
        RepositoryPath repoDataPath = repositoryPathResolver.resolve(repository, platformId + "/repodata.json");
//        RepoData repoData = condaRepoDataService.getRepoData(repoDataPath.getParent().toString());
        RepoData repoData = null;
        if (targetName.equals("repodata.json")) {
            repoData = condaRepoDataService.getRepoData(repoDataPath.getParent().toString());
        } else if (targetName.equals("current_repodata.json")) {
            repoData = condaRepoDataService.getCurrentRepoData(repoDataPath.getParent().toString());
        }

        // 2. 设置响应头
        response.setContentType(MediaType.APPLICATION_JSON_VALUE);
        response.setHeader("Content-Disposition", "attachment; filename=" + targetName);

        // 3. 写入响应
        try (PrintWriter writer = response.getWriter()) {
            writer.write(repoData.toJsonPretty());
            writer.flush();
        } catch (IOException e) {
            log.error("Error writing repodata.json to response", e);
            response.setStatus(HttpServletResponse.SC_INTERNAL_SERVER_ERROR);
        }
    }

    @ApiOperation(value = "Get conda package")
    @ApiResponses(value = {
            @ApiResponse(code = 200, message = "Get conda package successfully"),
            @ApiResponse(code = 404, message = "Conda package not found"),
            @ApiResponse(code = 500, message = "Internal server error")
    })
    @GetMapping(value = "/{storageId}/{repositoryId}/{platformId}/{packageName}",
                produces = MediaType.APPLICATION_OCTET_STREAM_VALUE)
    public void getCondaPackage(@RepositoryMapping Repository repository,
                                @PathVariable String platformId,
                                @PathVariable String packageName,
                                @RequestHeader HttpHeaders httpHeaders,
                                HttpServletRequest request,
                                HttpServletResponse response)
            throws Exception {
        // 1. 获取conda包路径
        RepositoryPath condaPackagePath = repositoryPathResolver.resolve(repository, platformId + "/" + packageName);
        // 2. 检查包是否存在
//        if (!Files.exists(condaPackagePath)) {
//            response.setStatus(HttpServletResponse.SC_NOT_FOUND);
//            return;
//        }
        if (!condaArtifactService.checkArtifactExist(condaPackagePath)) {
            response.setStatus(HttpServletResponse.SC_NOT_FOUND);
            return;
        }

        provideArtifactDownloadResponse(request, response, httpHeaders, condaPackagePath);
    }


    @ApiOperation(value = "Get conda package")
    @ApiResponses(value = {
            @ApiResponse(code = 200, message = "DELETE conda package successfully"),
            @ApiResponse(code = 404, message = "Conda package not found"),
            @ApiResponse(code = 500, message = "Internal server error")
    })
    @PreAuthorize("hasAuthority('ARTIFACTS_DEPLOY')")
    @DeleteMapping(value = {"/{storageId}/{repositoryId}/{platformId}/{packageName}",
                            "/{storageId}/{repositoryId}/package/{platformId}/{packageName}"})
    public ResponseEntity<?> deleteCondaPackage(@RepositoryMapping Repository repository,
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



    /**
     *
     * @Description: 存储conda包
     * @param repository: 存储库
     * @param coordinates: 坐标
     * @param artifactTempFile: 临时文件
     * @throws IOException
     * @throws ProviderImplementationException
     * @throws ArtifactCoordinatesValidationException
     */
    private void storeCondaPackage(Repository repository,
                                   @NonNull CondaArtifactCoordinates coordinates,
                                   @NonNull File artifactTempFile)
            throws IOException, ProviderImplementationException, ArtifactCoordinatesValidationException {

        RepositoryPath artifactPath = repositoryPathResolver.resolve(repository, coordinates);
        try (InputStream is = new FileInputStream(artifactTempFile)) {
            artifactManagementService.validateAndStore(artifactPath, is);
        }
    }
}
