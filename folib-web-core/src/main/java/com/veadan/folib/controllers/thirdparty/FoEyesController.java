package com.veadan.folib.controllers.thirdparty;

import com.veadan.folib.components.thirdparty.foeyes.FoEyesComponent;
import com.veadan.folib.components.thirdparty.foeyes.reponse.ProjectInfo;
import com.veadan.folib.domain.FoEyesConfig;
import com.veadan.folib.providers.io.RepositoryFiles;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.providers.io.RepositoryPathResolver;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.nio.file.Files;
import java.util.Objects;

/**
 * @author veadan
 * @date 2024/4/24
 **/
@RestController
@RequestMapping("/api/foEyes/")
public class FoEyesController {

    @Autowired
    private FoEyesComponent foEyesComponent;

    @Autowired
    private RepositoryPathResolver repositoryPathResolver;

    @PreAuthorize("hasAuthority('ARTIFACTS_RESOLVE')")
    @GetMapping(value = "/{storageId}/{repositoryId}/{artifactPath:.+}")
    public ResponseEntity<ProjectInfo> queryProject(@PathVariable String artifactPath,
                                                    @PathVariable String storageId,
                                                    @PathVariable String repositoryId) throws Exception {
        RepositoryPath repositoryPath = repositoryPathResolver.resolve(storageId, repositoryId, artifactPath);
        if (Objects.isNull(repositoryPath) || !Files.exists(repositoryPath)) {
            return ResponseEntity.ok(null);
        }
        String name = String.format("%s/%s/%s", storageId, repositoryId, RepositoryFiles.relativizePath(repositoryPath));
        return ResponseEntity.ok(foEyesComponent.queryProject(name));
    }

    @GetMapping(value = "/getConfig")
    public ResponseEntity<FoEyesConfig> getConfig() {
        return ResponseEntity.ok(foEyesComponent.getConfig());
    }
}
