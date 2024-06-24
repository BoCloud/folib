package com.veadan.folib.domain.gitls.command;

import javax.servlet.http.HttpServletRequest;

import com.veadan.folib.configuration.ConfigurationManager;
import com.veadan.folib.domain.gitls.model.GitLfsJson;
import com.veadan.folib.domain.gitls.utils.GitLfsHelper;
import com.veadan.folib.repositories.ArtifactRepository;
import lombok.Generated;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public abstract class LfsBaseUploadCommand {
    private static final Logger log = LoggerFactory.getLogger(LfsBaseUploadCommand.class);


    private ArtifactRepository artifactRepository;

    protected ConfigurationManager configurationManager;

    protected LfsBaseUploadCommand(ArtifactRepository artifactRepository, ConfigurationManager configurationManager) {
        this.artifactRepository = artifactRepository;
        this.configurationManager = configurationManager;
    }

    protected GitLfsJson createLfsUploadJson(GitLfsJson requestJson, String authHeader, String storageId, String repositoryId, String uploadPath) {
        GitLfsJson uploadResponse = new GitLfsJson(requestJson);
        if (isPathExists(storageId, repositoryId, uploadPath)) {
            return uploadResponse;
        }
        String baserUrl = configurationManager.getConfiguration().getBaseUrl();
        baserUrl = baserUrl.endsWith("/") ? baserUrl.substring(0, baserUrl.length() - 1) : baserUrl;
        uploadResponse.setUploadLink(GitLfsHelper.getArtifactLfsUrl(baserUrl, storageId, repositoryId, uploadPath, requestJson.getOid()));
        GitLfsHelper.addAuthHeaderIfPresent(uploadResponse.getUploadHeaders(), authHeader);
        GitLfsHelper.addChecksumVerificationHeader(uploadResponse.getUploadHeaders(), requestJson.getOid());
        return uploadResponse;
    }

    protected boolean isPathExists(String storageId, String repositoryId, String oidPath) {
        return this.artifactRepository.artifactExists(storageId, repositoryId, oidPath);
    }
}

