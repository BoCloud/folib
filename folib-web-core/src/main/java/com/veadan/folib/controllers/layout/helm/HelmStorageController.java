package com.veadan.folib.controllers.layout.helm;

import com.veadan.folib.controllers.BaseArtifactController;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.storage.repository.Repository;
import com.veadan.folib.web.LayoutRequestMapping;
import com.veadan.folib.web.RepositoryMapping;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.HttpHeaders;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.InputStream;
import java.io.OutputStream;
import java.nio.file.Files;

@LayoutRequestMapping("helm")
@RestController
@Slf4j
public class HelmStorageController extends BaseArtifactController {

    @ApiOperation(value = "Used to retrieve an artifact")
    @ApiResponses(value = {@ApiResponse(code = 200, message = ""),
            @ApiResponse(code = 404, message = "Requested path not found."),
            @ApiResponse(code = 500, message = "Server error."),
            @ApiResponse(code = 503, message = "Repository currently not in service.")})
    @PreAuthorize("hasAuthority('ARTIFACTS_RESOLVE')")
    @RequestMapping(value = {"/{storageId}/{repositoryId}/{artifactPath:.+}"}, method = {RequestMethod.GET, RequestMethod.HEAD})
    public void download(@RepositoryMapping Repository repository,
                         @RequestHeader HttpHeaders httpHeaders,
                         @PathVariable String artifactPath,
                         HttpServletRequest request,
                         HttpServletResponse response)
            throws Exception {
        final String storageId = repository.getStorage().getId();
        final String repositoryId = repository.getId();
        logger.info("Requested get helm {}/{}/{}.", storageId, repositoryId, artifactPath);
        RepositoryPath repositoryPath = repositoryPathResolver.resolve(storageId, repositoryId, artifactPath);
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
}
