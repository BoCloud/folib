package com.folib.domain.gitls.command.local;

import com.folib.configuration.ConfigurationManager;
import com.folib.domain.gitls.command.LfsBaseUploadCommand;

import com.folib.domain.gitls.model.GitLfsJson;
import com.folib.domain.gitls.utils.GitLfsHelper;
import com.folib.repositories.ArtifactRepository;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.ResponseEntity;

public class LocalUploadCommand extends LfsBaseUploadCommand {

    private static final Logger log = LoggerFactory.getLogger(LocalUploadCommand.class);

    public LocalUploadCommand(ArtifactRepository artifactRepository, ConfigurationManager configurationManager) {
        super(artifactRepository, configurationManager);
    }

    public ResponseEntity<?> upload(String storageId, String repositoryId, GitLfsJson lfsJson, String authHeader) {
        String uploadPath = GitLfsHelper.getOidPath(lfsJson.getOid());

        GitLfsJson uploadResponse = createLfsUploadJson(lfsJson, authHeader, storageId, repositoryId, uploadPath);
        return buildUploadResponse(storageId, repositoryId, uploadPath, uploadResponse);
    }

    private ResponseEntity<?> buildUploadResponse(String storageId, String repositoryId, String uploadPath, GitLfsJson uploadResponse) {

        if (StringUtils.isBlank(uploadResponse.getUploadLink())) {
            log.debug("oid in Path {}/{}:{} already exists - no need to re-upload", storageId, repositoryId, uploadPath);
            return ResponseEntity.status(200).body(uploadResponse);
        } else {
            return ResponseEntity.status(202).body(uploadResponse);
        }
    }
}

