package com.veadan.folib.domain.huggingface.command;

import java.io.IOException;
import java.io.InputStream;
import javax.inject.Inject;
import javax.ws.rs.core.Response;

import com.veadan.folib.domain.Artifact;
import com.veadan.folib.domain.huggingface.model.request.MlModelRequestContext;
import com.veadan.folib.domain.huggingface.utils.MlModelUtils;
import com.veadan.folib.providers.ProviderImplementationException;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.providers.io.RepositoryPathResolver;
import com.veadan.folib.repositories.ArtifactRepository;
import com.veadan.folib.services.ArtifactManagementService;
import com.veadan.folib.storage.validation.artifact.ArtifactCoordinatesValidationException;
import lombok.Generated;
import lombok.NonNull;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;

public class MlModelUploadLfsFilesCommand {

    private static final Logger log = LoggerFactory.getLogger(MlModelUploadLfsFilesCommand.class);

    protected RepositoryPathResolver repositoryPathResolver;

    protected ArtifactManagementService artifactManagementService;

    private ArtifactRepository artifactRepository;

    public MlModelUploadLfsFilesCommand(RepositoryPathResolver repositoryPathResolver,
                                        ArtifactManagementService artifactManagementService,
                                        ArtifactRepository artifactRepository) {
        this.repositoryPathResolver = repositoryPathResolver;
        this.artifactManagementService = artifactManagementService;
        this.artifactRepository = artifactRepository;
    }

    public ResponseEntity<?> uploadFile(MlModelRequestContext context, InputStream stream) {
        if (context == null) {
            throw new NullPointerException("context is marked non-null but is null");
        }
        if (stream == null) {
            throw new NullPointerException("stream is marked non-null but is null");
        }
        log.debug("Received upload lfs file request for repo/organization/modelName/file '{}'/'{}'/'{}'/'{}'", context
                .getRepositoryId(), context.getOrg(), context.getModelName(), context.getFile());
        try {
            String uploadPath = MlModelUtils.getLfsTmpUploadPath(context.getOrg(), context.getModelName(), context.getFile());
            Artifact artifact = artifactRepository.findOneArtifact(context.getStorageId(), context.getRepositoryId(), uploadPath);
            if (artifact == null) {
                RepositoryPath repositoryPath = repositoryPathResolver.resolve(context.getStorageId(), context.getRepositoryId(), uploadPath);
                artifactManagementService.validateAndStore(repositoryPath, stream);
            }
            return ResponseEntity.ok(uploadPath);
        } catch (IOException | ProviderImplementationException | ArtifactCoordinatesValidationException e) {
            log.error("Failed to upload file '{}'/'{}'/'{}'/'{}'", context.getRepositoryId(), context.getOrg(), context.getModelName(), context.getFile(), e);
            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).body(e.getMessage());
        }

        //PackageUploadResult uploadResult = this.uploadService.upload(context.getRepoKey(), uploadPath, stream);
        //this.securityService.callAsSystem(() -> {
        //    ArrayListMultimap arrayListMultimap = ArrayListMultimap.create();
        //    arrayListMultimap.put("hf_last_updated", String.valueOf(System.currentTimeMillis()));
        //    this.repositoryService.setAttributes(context.getRepoKey(), uploadPath, (Multimap)arrayListMultimap);
        //    return null;
        //});
        //return Response.ok(uploadResult.getPath()).build();

    }
}

