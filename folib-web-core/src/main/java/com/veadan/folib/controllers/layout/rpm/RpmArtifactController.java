package com.veadan.folib.controllers.layout.rpm;


import com.veadan.folib.annotation.AuditLog;
import com.veadan.folib.config.RepodataUtil;
import com.veadan.folib.controllers.BaseArtifactController;
import com.veadan.folib.entity.Dict;
import com.veadan.folib.enums.AuditEventNameEnum;
import com.veadan.folib.metadata.indexer.RpmGroupRepoIndexer;
import com.veadan.folib.metadata.indexer.RpmRepoIndexer;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.providers.layout.RpmLayoutProvider;
import com.veadan.folib.services.DictService;
import com.veadan.folib.services.RepositoryManagementService;
import com.veadan.folib.storage.repository.Repository;
import com.veadan.folib.web.LayoutRequestMapping;
import com.veadan.folib.web.RepositoryMapping;
import io.swagger.annotations.*;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.multipart.MultipartFile;

import javax.inject.Inject;
import javax.xml.stream.XMLStreamException;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;

import static org.springframework.http.HttpStatus.NOT_FOUND;

@RestController
@LayoutRequestMapping(RpmLayoutProvider.ALIAS)
@Slf4j
@Api(description = "rpm坐标控制器",tags = "rpm坐标控制器")
public class RpmArtifactController extends BaseArtifactController {

    @Autowired
    private RepodataUtil repodataUtil;

    @Autowired
    private DictService dictService;

    @Inject
    private RepositoryManagementService repositoryManagementService;

    @Value("${folib.temp}")
    private  String tempPath;

    private final Object lock = new Object();

    @Override
    @PreAuthorize("authenticated")
    @GetMapping(value = "/{storageId}/{repositoryId}")
    public ResponseEntity<String> checkRepositoryAccess() {
        return super.checkRepositoryAccess();
    }

    @ApiOperation(value = "Deletes a path from a repository.")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "The artifact was deleted."),
            @ApiResponse(code = 400, message = "Bad request."),
            @ApiResponse(code = 404, message = "The specified storageId/repositoryId/path does not exist!")})
    @PreAuthorize("hasAuthority('ARTIFACTS_DELETE')")
    @DeleteMapping(value = "/{storageId}/{repositoryId}/{artifactPath:.+}")
    public ResponseEntity delete(@RepositoryMapping Repository repository,
                                 @ApiParam(value = "Whether to use force delete")
                                 @RequestParam(defaultValue = "false",
                                         name = "force",
                                         required = false) boolean force,
                                 @PathVariable String artifactPath) {
        final String storageId = repository.getStorage().getId();
        final String repositoryId = repository.getId();
        logger.info("Deleting {}:{}/{}...", storageId, repositoryId, artifactPath);
        try {
            final RepositoryPath repositoryPath = repositoryPathResolver.resolve(storageId, repositoryId, artifactPath);
            if (!Files.exists(repositoryPath)) {
                return ResponseEntity.status(NOT_FOUND)
                        .body("The specified path does not exist!");
            }
            artifactManagementService.delete(repositoryPath, force);
            // 刷新索引
            RepositoryPath repoPath = repositoryPathResolver.resolve(repository, "");
            String absolutePath = repoPath.toAbsolutePath().toString();
            //repodataUtil.updateIndex(absolutePath);
            RpmRepoIndexer rpmRepoIndexer = new RpmRepoIndexer(repositoryPathResolver,artifactManagementService,tempPath);
            rpmRepoIndexer.indexWriter(repository);
        } catch (Exception e) {
            logger.error(e.getMessage(), e);
            return ResponseEntity.status(HttpStatus.BAD_REQUEST)
                    .body(e.getMessage());
        }
        return ResponseEntity.ok("The artifact was deleted.");
    }

    @AuditLog(value = AuditEventNameEnum.UPLOAD_ARTIfFACT,target ="#repository.getStorage().getId() + '/' + #repository.getId() + '/' + #path + '/' +  #files[0].getOriginalFilename()")
    @ApiOperation(value = "Used to deploy an artifact")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "The artifact was deployed successfully."),
            @ApiResponse(code = 400, message = "An error occurred.")})
    @PreAuthorize("hasAuthority('ARTIFACTS_DEPLOY')")
    @RequestMapping(value = "{storageId}/{repositoryId}/{path:.+}", method = {RequestMethod.PUT, RequestMethod.POST})
    public ResponseEntity upload(@RepositoryMapping Repository repository,
                                 @PathVariable String path,
                                 HttpServletRequest request,
                                 @RequestParam("files") MultipartFile[] files,
                                 @RequestParam(name = "uuid", required = false) String uuid) {
        final String storageId = repository.getStorage().getId();
        final String repositoryId = repository.getId();
        try {
            for (MultipartFile multipartFile : files) {
                if (multipartFile.isEmpty()) {
                    continue;
                }
                String filename = multipartFile.getOriginalFilename();
                String rpmPath = "Packages/" + filename;
                RepositoryPath repositoryPath = repositoryPathResolver.resolve(storageId, repositoryId, rpmPath);
                try (InputStream is =  multipartFile.getInputStream()){
                    artifactManagementService.store(repositoryPath, is);
                }

            }

            RepositoryPath repoPath = repositoryPathResolver.resolve(repository, "repodata");
            RpmRepoIndexer rpmRepoIndexer = new RpmRepoIndexer(repositoryPathResolver,artifactManagementService,tempPath);
            if (!Files.exists(repoPath)) {
                synchronized (lock) {
                    RepositoryPath reposPath = repositoryPathResolver.resolve(repository, "");
                    String absolutePath = reposPath.toAbsolutePath().toString();
                   // repodataUtil.createRepo(absolutePath);
                    rpmRepoIndexer.indexWriter(repository);
                }
            } else {
                // 刷新索引
                RepositoryPath repositoryPath = repositoryPathResolver.resolve(repository, "");
                String absolutePath = repositoryPath.toAbsolutePath().toString();
                //repodataUtil.updateIndex(absolutePath);
                rpmRepoIndexer.indexWriter(repository);
            }
            return ResponseEntity.ok("The artifact was deployed successfully.");
        } catch (Exception e) {
            logger.error(e.getMessage(), e);
            if (StringUtils.isNotBlank(uuid)) {
                String message = e.getMessage();
                if (StringUtils.isBlank(message)) {
                    message = "未知异常";
                }
                dictService.saveOrUpdateDict(Dict.builder().dictKey(uuid).comment(message).build(), null);
            }
            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).body(e.getMessage());
        }
    }

    @ApiOperation(value = "Used to build rpm local repository atficat index")
    @ApiResponses(value = {@ApiResponse(code = 200, message = ""),
            @ApiResponse(code = 400, message = "An error occurred.")})
    @PreAuthorize("hasAuthority('ARTIFACTS_DEPLOY')")
    @GetMapping(value = {"{storageId}/{repositoryId}/buildIndex"})
    public ResponseEntity buildIndex(@RepositoryMapping Repository repository) {
        try {
            RpmRepoIndexer rpmRepoIndexer = new RpmRepoIndexer(repositoryPathResolver,artifactManagementService,tempPath);
            RepositoryPath repositoryPath = repositoryPathResolver.resolve(repository, "");
            String absolutePath = repositoryPath.toAbsolutePath().toString();
            repodataUtil.deleteRepo(absolutePath + "/repodata");
            //repodataUtil.createRepo(absolutePath);
            rpmRepoIndexer.indexWriter(repository);
            return ResponseEntity.ok("The rpm repository was build index successfully.");
        } catch (Exception e) {
            log.error("Build index err {}", e.getMessage());
            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).body(e.getMessage());
        }
    }

    @ApiOperation(value = "Used to retrieve an artifact")
    @ApiResponses(value = {@ApiResponse(code = 200, message = ""),
            @ApiResponse(code = 400, message = "An error occurred.")})
    @PreAuthorize("hasAuthority('ARTIFACTS_RESOLVE')")
    @AuditLog(value = AuditEventNameEnum.DOWNLOAD_EXCEPTION, target = "#storageId + '/' + #repositoryId + '/' + #path")
    @GetMapping(value = {"{storageId}/{repositoryId}/{path:.+}"})
    public void download(@RepositoryMapping Repository repository,
                         @RequestHeader HttpHeaders httpHeaders,
                         @PathVariable String path,
                         @PathVariable String storageId,
                         @PathVariable String repositoryId,
                         HttpServletRequest request,
                         HttpServletResponse response)
            throws Exception {
        logger.info("Requested /{}/{}/{}.", storageId, repositoryId, path);
        RepositoryPath repositoryPath = artifactResolutionService.resolvePath(storageId, repositoryId, path);
        vulnerabilityBlock(repositoryPath);
        provideArtifactDownloadResponse(request, response, httpHeaders, repositoryPath);
    }
}