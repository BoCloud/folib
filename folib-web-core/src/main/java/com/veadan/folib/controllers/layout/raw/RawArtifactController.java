package com.veadan.folib.controllers.layout.raw;

import com.veadan.folib.annotation.AuditLog;
import com.veadan.folib.components.ArtifactSecurityComponent;
import com.veadan.folib.controllers.BaseArtifactController;
import com.veadan.folib.enums.AuditEventNameEnum;
import com.veadan.folib.enums.ProductTypeEnum;
import com.veadan.folib.providers.io.RepositoryFiles;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.providers.layout.RawLayoutProvider;
import com.veadan.folib.storage.repository.Repository;
import com.veadan.folib.users.domain.Privileges;
import com.veadan.folib.util.RepositoryPathUtil;
import com.veadan.folib.web.LayoutRequestMapping;
import com.veadan.folib.web.RepositoryMapping;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;
import org.apache.commons.collections4.CollectionUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Lazy;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.ui.ModelMap;
import org.springframework.web.bind.annotation.*;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.*;
import java.nio.file.attribute.BasicFileAttributes;
import java.util.Comparator;
import java.util.Objects;
import java.util.UUID;
import java.util.zip.ZipEntry;
import java.util.zip.ZipOutputStream;

/**
 * @author Veadan
 */
@RestController
@LayoutRequestMapping(RawLayoutProvider.ALIAS)
@Api(description = "raw坐标控制器", tags = "raw坐标控制器")
public class RawArtifactController
        extends BaseArtifactController {

    @Value("${folib.temp}")
    private String tempPath;

    @Autowired
    @Lazy
    private ArtifactSecurityComponent artifactSecurityComponent;

    @Override
    @PreAuthorize("authenticated")
    @GetMapping(value = {"/{repositoryId}/"})
    public ResponseEntity<String> checkRepositoryAccess() {
        return super.checkRepositoryAccess();
    }

    @AuditLog(value = AuditEventNameEnum.UPLOAD_ARTIfFACT, target = "#repository.getStorage().getId() + '/' + #repository.getId() + '/' + #path")
    @ApiOperation(value = "Used to deploy an artifact")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "The artifact was deployed successfully."),
            @ApiResponse(code = 400, message = "An error occurred.")})
    @PreAuthorize("hasAuthority('ARTIFACTS_DEPLOY')")
    @PutMapping(value = {"/{repositoryId}/{path:.+}"})
    public ResponseEntity upload(@RepositoryMapping Repository repository,
                                 @PathVariable String path,
                                 HttpServletRequest request) {
        final String storageId = repository.getStorage().getId();
        final String repositoryId = repository.getId();

        try (InputStream inputStream = request.getInputStream()) {
            RepositoryPath repositoryPath = repositoryPathResolver.resolve(storageId, repositoryId, path);
            artifactManagementService.validateAndStore(repositoryPath, inputStream);

            return ResponseEntity.ok("The artifact was deployed successfully.");
        } catch (Exception e) {
            logger.error(e.getMessage(), e);

            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).body(e.getMessage());
        }
    }

    @ApiOperation(value = "Used to retrieve an artifact")
    @ApiResponses(value = {@ApiResponse(code = 200, message = ""),
            @ApiResponse(code = 400, message = "An error occurred.")})
    @PreAuthorize("hasAuthority('ARTIFACTS_VIEW')")
    @AuditLog(value = AuditEventNameEnum.DOWNLOAD_EXCEPTION, target = "#repository.getStorage().getId() + '/' + #repository.getId() + '/' + #path")
    @GetMapping(value = {"/{repositoryId}/{path:.+}"})
    public Object download(@RepositoryMapping Repository repository,
                           @RequestHeader HttpHeaders httpHeaders,
                           @PathVariable String path,
                           @RequestParam(required = false) Boolean isZip,
                           HttpServletRequest request,
                           HttpServletResponse response,
                           ModelMap model)
            throws Exception {
        if (Boolean.TRUE.equals(isZip)) {
            RepositoryPath repositoryPath = repositoryPathResolver.resolve(repository.getStorage().getId(), repository.getId(), path);
            if (Objects.nonNull(repositoryPath) && Files.exists(repositoryPath) && Files.isDirectory(repositoryPath) && artifactSecurityComponent.validatePrivileges(repositoryPath, Privileges.ARTIFACTS_RESOLVE.getAuthority())) {
                String uuid = UUID.randomUUID().toString();
                Path tempPaths = Paths.get(String.join("/", tempPath, uuid));
                try {
                    Files.createDirectories(tempPaths);
                    String baseDirPath = getLastDirectoryDirect(repositoryPath);
                    Path zipFilePath = Paths.get(String.join("/", tempPaths.toString(), baseDirPath + ".zip"));
                    zipDirectory(repositoryPath, zipFilePath.toString());
                    response.setHeader(HttpHeaders.CONTENT_DISPOSITION, "attachment; filename=\"" + zipFilePath.getFileName().toString() + "\"");
                    response.setContentType("application/octet-stream");
                    copyToResponse(Files.newInputStream(Path.of(zipFilePath.toString())), response);
                } finally {
                    Files.walk(tempPaths)
                            .sorted(Comparator.reverseOrder())
                            .map(Path::toFile)
                            .forEach(File::delete);
                }
                return null;
            }
            response.setStatus(HttpStatus.NOT_FOUND.value());
            return null;
        }
        return super.download(repository, httpHeaders, path, request, response, model);
    }

    public void zipDirectory(RepositoryPath sourceDirPath, String zipFilePath) throws IOException {
        try (ZipOutputStream zos = new ZipOutputStream(new FileOutputStream(zipFilePath))) {
            Files.walkFileTree(sourceDirPath, new SimpleFileVisitor<Path>() {
                @Override
                public FileVisitResult visitFileFailed(Path file, IOException exc) throws IOException {
                    if (exc instanceof NoSuchFileException) {
                        logger.warn("Find path [{}] no such file skip...", file);
                        return FileVisitResult.CONTINUE;
                    }
                    return super.visitFileFailed(file, exc);
                }

                @Override
                public FileVisitResult visitFile(Path file,
                                                 BasicFileAttributes attrs)
                        throws IOException {
                    RepositoryPath itemPath = (RepositoryPath) file;
                    if (RepositoryPathUtil.include(1, itemPath, false, ProductTypeEnum.Raw.getFoLibraryName())) {
                        zos.putNextEntry(new ZipEntry(RepositoryFiles.relativizePath(itemPath)));
                        Files.copy(file, zos);
                        zos.closeEntry();
                    }
                    return FileVisitResult.CONTINUE;
                }

                @Override
                public FileVisitResult preVisitDirectory(final Path dir, final BasicFileAttributes attrs) throws IOException {
                    try {
                        RepositoryPath itemPath = (RepositoryPath) dir;
                        if (!Files.isSameFile(itemPath, itemPath.getRoot()) && !RepositoryPathUtil.include(2, itemPath, false, ProductTypeEnum.Raw.getFoLibraryName())) {
                            return FileVisitResult.SKIP_SUBTREE;
                        }
                        zos.putNextEntry(new ZipEntry(RepositoryFiles.relativizePath(itemPath) + "/"));
                        zos.closeEntry();
                        return FileVisitResult.CONTINUE;
                    } catch (NoSuchFileException e) {
                        // 文件已删除，跳过处理
                        logger.warn("Find path directory [{}] no such directory skip...", dir);
                        return FileVisitResult.CONTINUE;
                    }
                }
            });
        }
    }

    private String getLastDirectoryDirect(Path path) {
        int nameCount = path.getNameCount();
        if (nameCount > 0) {
            return path.subpath(nameCount - 1, nameCount).toString();
        } else {
            // 如果路径为空或只包含一个名称（可能是文件），则返回null或适当的值
            return null;
        }
    }

}
