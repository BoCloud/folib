package com.veadan.folib.controllers.layout.conda;

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
import com.veadan.folib.services.CondaRepoDataService;
import com.veadan.folib.storage.repository.Repository;
import com.veadan.folib.storage.validation.artifact.ArtifactCoordinatesValidationException;
import com.veadan.folib.web.LayoutRequestMapping;

import com.veadan.folib.web.RepositoryMapping;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;


import lombok.NonNull;
import lombok.extern.slf4j.Slf4j;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
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
import java.util.Optional;


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
    private CondaMetadataExtractor condaMetadataExtractor;

    @Inject
    private ArtifactManagementService artifactManagementService;

    @Inject
    private RepositoryPathResolver repositoryPathResolver;


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

        RepositoryPath tmpPath = null;
        try {
            // 1. 存储到临时目录
            tmpPath = repositoryPathResolver.resolve(repository, "tmp/" + fileName);
            File tmpDir = new File(tmpPath.getParent().toString());
            if (!tmpDir.exists()) {
                tmpDir.mkdirs();
            }

            File tmpFile = new File(tmpPath.toString());
            try(FileOutputStream fos = new FileOutputStream(tmpFile)) {
                fos.write(file.getBytes());
            } catch (IOException e) {
                return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).body("Failed to save file: " + e.getMessage());
            }

            // 2. 提取元数据
            Index index = condaMetadataExtractor.extract(tmpPath.getParent().toString(), fileName);
            if (index == null) {
                throw new RuntimeException("Failed to extract metadata");
            }

            // 3. 获取platform
            String platform = Optional.ofNullable(index.getSubdir()).orElse("noarch");

            // 4. 构建conda路径
            RepositoryPath artifactPath = repositoryPathResolver.resolve(repository, platform + "/" + fileName);

            // 5. 检查文件是否存在
            if (checkArtifactExist(artifactPath)) {
                return ResponseEntity.status(HttpStatus.CONFLICT).body("Artifact already exists");
            }

            // 6. 存储文件
            CondaArtifactCoordinates coordinates = CondaArtifactCoordinates.of(platform, fileName);
            storeCondaPackage(repository, coordinates, tmpPath);

            // 7. 更新索引
            condaRepoDataService.sendRepoDataEvent(RepoDataEventKind.ADD, artifactPath.getParent().toString(), fileName);

            return ResponseEntity.ok("Artifact uploaded successfully");
        } catch (Exception e) {
            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).body("Processing error: " + e.getMessage());
        } finally {
            // 9. 安全删除临时文件
            if (tmpPath != null && Files.exists(tmpPath)) {
                try {
                    Files.delete(tmpPath);
                } catch (IOException e) {
                    log.error("Failed to delete tmp file at: {}", tmpPath, e);
                }
            }
        }
    }

    @ApiOperation(value = "Get repodata")
    @ApiResponses(value = {
            @ApiResponse(code = 200, message = "Get repodata successfully"),
            @ApiResponse(code = 404, message = "Repodata not found"),
            @ApiResponse(code = 500, message = "Internal server error")
    })
    @GetMapping(value = "/{storageId}/{repositoryId}/{platformId}/repodata.json",
                produces = MediaType.MULTIPART_FORM_DATA_VALUE)
    public void getRepoData(@RepositoryMapping Repository repository,
                                         @PathVariable String platformId,
                                         HttpServletResponse response) {
        // 1. 获取RepoData
        RepositoryPath repoDataPath = repositoryPathResolver.resolve(repository, platformId + "/repodata.json");
        RepoData repoData = condaRepoDataService.getRepoData(repoDataPath.getParent().toString());

        // 2. 设置响应头
        response.setContentType(MediaType.APPLICATION_JSON_VALUE);
        response.setHeader("Content-Disposition", "attachment; filename=repodata.json");

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
        if (!Files.exists(condaPackagePath)) {
            response.setStatus(HttpServletResponse.SC_NOT_FOUND);
            return;
        }

        provideArtifactDownloadResponse(request, response, httpHeaders, condaPackagePath);
    }


    /**
     * @Description: 检查包是否存在, FileExist and IndexExist. 若不一致, 则修复(删除索引或添加索引)
     * @param path: 完整的文件路径, repoKey/artifactName
     * @return
     */
    private boolean checkArtifactExist(@NonNull RepositoryPath path) throws Exception {
        boolean fileExist = false;

        boolean indexExist = false;
        // 1. 提取父目录和文件名和indexPath
        String parentPath = path.getParent().toString();
        String fileName = path.getFileName().toString();

        // 2. 检查文件是否存在
        fileExist = Files.exists(path);

        // 3. 检查索引文件是否存在, 且文件是否在索引中
        RepoData repoData = condaRepoDataService.getRepoData(parentPath);
        indexExist = condaRepoDataService.checkPackageExistsInRepoData(repoData, fileName);

        if (fileExist && indexExist) {
            return true;
        } else if (!fileExist && !indexExist) {
            return false;
        } else if (fileExist && !indexExist) {
            // 4. 文件存在, 索引不存在, 则添加索引
            condaRepoDataService.sendRepoDataEvent(RepoDataEventKind.ADD, parentPath, fileName);
            return true;
        } else if (!fileExist && indexExist) {
            // 5. 文件不存在, 索引存在, 则删除索引
            condaRepoDataService.sendRepoDataEvent(RepoDataEventKind.REMOVE, parentPath, fileName);
            return false;
        }
        return false;
    }



    /**
     *
     * @Description: 存储conda包
     * @param repository: 存储库
     * @param coordinates: 坐标
     * @param condaPackageTmp: 临时文件路径
     * @throws IOException
     * @throws ProviderImplementationException
     * @throws ArtifactCoordinatesValidationException
     */
    private void storeCondaPackage(Repository repository,
                                   CondaArtifactCoordinates coordinates,
                                   Path condaPackageTmp)
            throws IOException, ProviderImplementationException, ArtifactCoordinatesValidationException {
        RepositoryPath artifactPath = repositoryPathResolver.resolve(repository, coordinates);
        try (InputStream is = new BufferedInputStream(Files.newInputStream(condaPackageTmp))) {
            artifactManagementService.validateAndStore(artifactPath, is);
        }
    }
}
