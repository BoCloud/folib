package com.veadan.folib.domain.gitls.command.local;


import com.veadan.folib.configuration.ConfigurationManager;
import com.veadan.folib.domain.gitls.command.LfsDownloadBaseCommand;
import com.veadan.folib.domain.gitls.utils.GitLfsHelper;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.repositories.ArtifactRepository;
import com.veadan.folib.services.ArtifactResolutionService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.ResponseEntity;

import javax.inject.Inject;
import javax.ws.rs.core.Response;
import java.io.IOException;

public class LocalDownloadCommand extends LfsDownloadBaseCommand {

    private static final Logger log = LoggerFactory.getLogger(LocalDownloadCommand.class);

    protected ArtifactResolutionService artifactResolutionService;
    public LocalDownloadCommand(ConfigurationManager configurationManager, ArtifactRepository artifactRepository,ArtifactResolutionService artifactResolutionServic) {
       super(configurationManager, artifactRepository);
       this.artifactResolutionService = artifactResolutionServic;
    }

    public ResponseEntity<?> download(String storageId, String repositoryId, String oid, String authHeader) throws IOException {
        String oidPath = GitLfsHelper.getOidPath(oid);
        ResponseEntity<?> response = verifyCanDownload(oid, storageId,repositoryId, oidPath, true);
        if (response == null) {
            RepositoryPath repositoryPath = artifactResolutionService.resolvePath(storageId, repositoryId, oidPath);
            //(String oid, String storageId, String repositoryId, String oidPath, String authHeader, Long size
            response = createLfsDownloadResponse(oid, storageId,repositoryId, oidPath, authHeader, repositoryPath.getArtifactEntry().getSizeInBytes());
        }
        return response;
    }
}
