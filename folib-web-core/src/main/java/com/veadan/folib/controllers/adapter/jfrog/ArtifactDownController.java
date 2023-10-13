package com.veadan.folib.controllers.adapter.jfrog;

import com.veadan.folib.providers.io.RepositoryPath;
import io.swagger.annotations.Api;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.HttpHeaders;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

/**
 * @author leipenghui
 */
@Slf4j
@RequestMapping("/artifactory")
@RestController
@PreAuthorize("hasAuthority('ADMIN')")
@Api(description = "JFrog下载", tags = "JFrog下载")
public class ArtifactDownController extends JFrogBaseController {

    @PreAuthorize("authenticated")
    @GetMapping(value = "/{repositoryId}/{artifactPath:.+}")
    public ResponseEntity download(@PathVariable("repositoryId") String repositoryId, @RequestHeader HttpHeaders httpHeaders, @PathVariable String artifactPath,
                                   HttpServletRequest request, HttpServletResponse response) throws Exception {
        final String storageId = getDefaultStorageId();
        boolean checkRepository = checkRepository(storageId, repositoryId);
        if (!checkRepository) {
            return repositoryNotFound();
        }
        logger.info("Requested /{}/{}/{}.", storageId, repositoryId, artifactPath);
        RepositoryPath repositoryPath = artifactResolutionService.resolvePath(storageId, repositoryId, artifactPath);

        vulnerabilityBlock(repositoryPath);
        provideArtifactDownloadResponse(request, response, httpHeaders, repositoryPath);

        return ResponseEntity.ok("ok");
    }
}
