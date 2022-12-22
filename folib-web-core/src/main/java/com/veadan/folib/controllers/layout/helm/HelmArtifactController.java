package com.veadan.folib.controllers.layout.helm;

import com.veadan.folib.config.HelmRepoUtil;
import com.veadan.folib.controllers.BaseArtifactController;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.providers.repository.proxied.ProxyRepositoryArtifactResolver;
import com.veadan.folib.storage.repository.Repository;
import com.veadan.folib.web.RepositoryMapping;
import io.swagger.annotations.*;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.multipart.MultipartFile;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.*;
import java.nio.file.Files;


/**
 * Helm 布局逻辑控制层
 *
 * @author qijianping
 */
//@LayoutRequestMapping("helm")
@RestController
public class HelmArtifactController extends BaseArtifactController {

    @Autowired
    private HelmRepoUtil helmRepoUtil;

    @Autowired
    private ProxyRepositoryArtifactResolver proxyRepositoryArtifactResolver;

    @ApiOperation(value = "Used to retrieve an artifact")
    @ApiResponses(value = {@ApiResponse(code = 200, message = ""),
            @ApiResponse(code = 400, message = "An error occurred.")})
    @PreAuthorize("hasAuthority('ARTIFACTS_RESOLVE')")
    @GetMapping(value = {"storages/{storageId}/{repositoryId}/{path:.+}"})
    public void downloadArtifact(@RepositoryMapping Repository repository,
                                 @RequestHeader HttpHeaders httpHeaders,
                                 @PathVariable String path,
                                 HttpServletRequest request,
                                 HttpServletResponse response) throws IOException {
        final String storageId = repository.getStorage().getId();
        final String repositoryId = repository.getId();
        logger.debug("Requested /{}/{}/{}.", storageId, repositoryId, path);
        RepositoryPath repositoryPath = repositoryPathResolver.resolve(storageId, repositoryId, path);
        vulnerabilityBlock(repositoryPath);
        try (InputStream in = Files.newInputStream(repositoryPath);) {
            OutputStream out = response.getOutputStream();
            response.setCharacterEncoding("UTF-8");
            // 设置文件头：设置下载文件名
            response.setHeader("Content-Disposition", "attachment;" + repositoryPath.getFileName().toString());
            int byteRead = 0;
            byte[] buffer = new byte[1024];
            while ((byteRead = in.read(buffer)) != -1) {
                out.write(buffer, 0, byteRead);
            }
            out.flush();
        } catch (Exception e) {
            logger.error("download helm artifact error {}", e.getMessage());
        }

    }


    @ApiOperation(value = "Used to retrieve an artifact")
    @ApiResponses(value = {@ApiResponse(code = 200, message = ""),
            @ApiResponse(code = 400, message = "An error occurred.")})
    @PreAuthorize("hasAuthority('ARTIFACTS_RESOLVE')")
    @GetMapping(value = {"{storageId}/{repositoryId}/{path}"})
    public void download(@RepositoryMapping Repository repository,
                         @RequestHeader HttpHeaders httpHeaders,
                         @PathVariable String path,
                         HttpServletRequest request,
                         HttpServletResponse response)
            throws Exception {
        try {
            final String storageId = repository.getStorage().getId();
            final String repositoryId = repository.getId();
            logger.debug("Requested /{}/{}/{}.", storageId, repositoryId, path);

            RepositoryPath repositoryPath = artifactResolutionService.resolvePath(storageId, repositoryId, path);
            if (null != repositoryPath && repository.getType().equalsIgnoreCase("proxy")
                    && path.equals("index.yaml") && repository.getLayout().equalsIgnoreCase("helm")) {
                if (Files.exists(repositoryPath)) {
                    proxyRepositoryArtifactResolver.fetchRemoteResource(repositoryPath);
                }
            }

            if (null == repositoryPath && repository.getType().equals("hosted") && path.equals("index.yaml")) {
                repositoryPath = repositoryPathResolver.resolve(storageId, repositoryId, path);
                if (!Files.exists(repositoryPath)) {
                    // 创建刷新索引
                    RepositoryPath repoPath = repositoryPathResolver.resolve(repository, "");
                    String absolutePath = repoPath.toAbsolutePath().toString();
                    helmRepoUtil.createIndex(absolutePath, repository);
                }
                try (InputStream in = Files.newInputStream(repositoryPath);) {
                    OutputStream out = response.getOutputStream();
                    response.setCharacterEncoding("UTF-8");
                    // 设置文件头：设置下载文件名
                    response.setHeader("Content-Disposition", "attachment;" + " filename=index.yaml");
                    int byteRead = 0;
                    byte[] buffer = new byte[1024];
                    while ((byteRead = in.read(buffer)) != -1) {
                        out.write(buffer, 0, byteRead);
                    }
                    out.flush();
                    return;
                }
            }
            vulnerabilityBlock(repositoryPath);
            provideArtifactDownloadResponse(request, response, httpHeaders, repositoryPath);
//            else {
//                vulnerabilityBlock(repositoryPath);
//                provideArtifactDownloadResponse(request, response, httpHeaders, repositoryPath);
//            }
        } catch (Exception e) {
            e.printStackTrace();
            logger.error("download helm artifact error {}", e.getMessage());
        }

    }

    @ApiOperation(value = "Used to deploy an artifact")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "The artifact was deployed successfully."),
            @ApiResponse(code = 400, message = "An error occurred.")})
    @PreAuthorize("hasAuthority('ARTIFACTS_RESOLVE')")
    @RequestMapping(value = "api/{storageId}/{repositoryId}/charts", method = {RequestMethod.POST})
    public ResponseEntity upload(@RepositoryMapping Repository repository,
                                 HttpServletRequest request,
                                 HttpServletResponse response,
                                 @RequestParam("chart") MultipartFile[] charts) {
        final String storageId = repository.getStorage().getId();
        final String repositoryId = repository.getId();
        try {
            for (MultipartFile file : charts) {
                String[] strArray = file.getOriginalFilename().split("/");
                String fileName = strArray[strArray.length - 1];
                RepositoryPath repositoryPath = repositoryPathResolver.resolve(storageId, repositoryId, fileName);
                artifactManagementService.store(repositoryPath, file.getInputStream());
            }
            // 创建刷新索引
            RepositoryPath repoPath = repositoryPathResolver.resolve(repository, "");
            String absolutePath = repoPath.toAbsolutePath().toString();
            helmRepoUtil.createIndex(absolutePath, repository);
            return ResponseEntity.ok("The artifact was deployed successfully.");
        } catch (Exception e) {
            logger.error(e.getMessage(), e);
            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).body(e.getMessage());
        }

    }

}
