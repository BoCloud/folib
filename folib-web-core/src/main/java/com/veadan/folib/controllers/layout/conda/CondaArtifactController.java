package com.veadan.folib.controllers.layout.conda;

import com.veadan.folib.artifact.coordinates.CondaArtifactCoordinates;
import com.veadan.folib.conda.model.Index;
import com.veadan.folib.conda.model.RepoDataEventKind;
import com.veadan.folib.conda.services.CondaRepoDataService;
import com.veadan.folib.controllers.BaseArtifactController;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.providers.io.RepositoryPathResolver;
import com.veadan.folib.services.ArtifactManagementService;
import com.veadan.folib.storage.repository.Repository;
import com.veadan.folib.web.LayoutRequestMapping;
import com.veadan.folib.conda.indexer.CondaMetadataExtractor;
import com.veadan.folib.conda.indexer.CondaMetadataIndexer;
import com.veadan.folib.conda.model.RepoData;

import com.veadan.folib.web.RepositoryMapping;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;


import lombok.NonNull;
import lombok.extern.slf4j.Slf4j;

import org.springframework.beans.factory.annotation.Autowired;
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

            // 6. 移动文件到目标路径
            // 6.1 检查父目录是否存在
            if (!Files.exists(artifactPath.getParent())) {
                Files.createDirectories(artifactPath.getParent());
            }
            // 6.2 移动文件
            Path targetPath = Path.of(artifactPath.toString());
            Path sourcePath = Path.of(tmpPath.toString());
            Files.move(sourcePath, targetPath, StandardCopyOption.REPLACE_EXISTING);

            // 7. 添加索引
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
     * @Description: 删除索引和文件
     * @param path: 完整的文件路径, repoKey/artifactName
     * @return
     */
    private void deleteArtifact(@NonNull RepositoryPath path) {

    }
}
