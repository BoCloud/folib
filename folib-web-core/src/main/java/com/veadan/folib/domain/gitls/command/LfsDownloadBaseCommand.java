package com.veadan.folib.domain.gitls.command;

import com.veadan.folib.configuration.ConfigurationManager;
import com.veadan.folib.domain.gitls.model.GitLfsJson;
import com.veadan.folib.domain.gitls.utils.GitLfsHelper;
import com.veadan.folib.repositories.ArtifactRepository;
import com.veadan.folib.users.userdetails.SpringSecurityUser;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.ResponseEntity;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContextHolder;

import javax.annotation.Nonnull;
import javax.ws.rs.core.Response;


public abstract class LfsDownloadBaseCommand {

    private static final Logger log = LoggerFactory.getLogger(LfsDownloadBaseCommand.class);

    protected ConfigurationManager configurationManager;
    private ArtifactRepository artifactRepository;

    protected LfsDownloadBaseCommand(ConfigurationManager configurationManager, ArtifactRepository artifactRepository) {
        this.configurationManager = configurationManager;
        this.artifactRepository = artifactRepository;
    }


    protected ResponseEntity<?> verifyCanDownload(String oid, String storageId, String repositoryId, String oidPath, boolean checkExistence) {
        if (checkExistence && !this.artifactRepository.artifactExists(storageId, repositoryId, oidPath)) {
            return notExistResponse(oid);
        } else {
            return null;
        }
    }


    protected ResponseEntity<?> createLfsDownloadResponse(String oid, String storageId, String repositoryId, String oidPath, String authHeader, Long size) {
        String baserUrl = configurationManager.getConfiguration().getBaseUrl();
        baserUrl = baserUrl.endsWith("/") ? baserUrl.substring(0, baserUrl.length() - 1) : baserUrl;
        return ResponseEntity.ok(GitLfsHelper.createLfsDownloadJson(oid, storageId, repositoryId, oidPath, authHeader, size, baserUrl));
    }

    protected static ResponseEntity<?> notExistResponse(String oid) {
        return ResponseEntity.status(404).body("oid " + oid + " doesn't exist.");
    }

    //protected boolean isPathExists(String repoKey, String oidPath) {
    //    try {
    //        return this.repositoryService.exists(repoKey, oidPath);
    //    } catch (PackageException e) {
    //        log.debug("Unable to verify existence of {}:{}, fallback to full verification", new Object[] { repoKey, oidPath, e });
    //        return false;
    //    }
    //}
    //
    //protected Response handleGetPackageException(String repoKey, String oid, String oidPath, PackageException e) {
    //    Response response;
    //    if (e.getStatus() == 403 || e.getStatus() == 401) {
    //        response = returnNoReadPermissionsResponse(oid, repoKey, oidPath);
    //    } else {
    //        log.debug("Unable to resolve artifact", (Throwable)e);
    //        response = notExistResponse(oid);
    //    }
    //    return response;
    //}
}
