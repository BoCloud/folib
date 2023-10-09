package com.veadan.folib.controllers.adapter.jfrog;

import com.veadan.folib.controllers.BaseArtifactController;
import com.veadan.folib.controllers.BaseController;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.storage.repository.Repository;
import com.veadan.folib.web.RepositoryMapping;
import io.swagger.annotations.Api;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.HttpHeaders;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

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
public class ArtifactDownController extends BaseArtifactController 
{

    @PreAuthorize("authenticated")
    @GetMapping(value = "/{storageId}/{repositoryId}/{artifactPath:.+}")
    public ResponseEntity download(@RepositoryMapping Repository repository, @RequestHeader HttpHeaders httpHeaders, @PathVariable String artifactPath, 
                                   HttpServletRequest request, HttpServletResponse response) throws Exception 
    {
        final String storageId = repository.getStorage().getId();
        final String repositoryId = repository.getId();
        logger.info("Requested /{}/{}/{}.", storageId, repositoryId, artifactPath);
        RepositoryPath repositoryPath = artifactResolutionService.resolvePath(storageId, repositoryId, artifactPath);

        vulnerabilityBlock(repositoryPath);
        provideArtifactDownloadResponse(request, response, httpHeaders, repositoryPath);
        
        return ResponseEntity.ok("ok");
    }
}
