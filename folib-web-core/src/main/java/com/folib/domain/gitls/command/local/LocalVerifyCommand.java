package com.folib.domain.gitls.command.local;

import com.folib.configuration.ConfigurationManager;
import com.folib.domain.gitls.command.LfsDownloadBaseCommand;
import com.folib.domain.gitls.utils.GitLfsHelper;
import com.folib.repositories.ArtifactRepository;
import lombok.Generated;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.ResponseEntity;

public class LocalVerifyCommand extends LfsDownloadBaseCommand {
    @Generated
    private static final Logger log = LoggerFactory.getLogger(LocalVerifyCommand.class);

    public LocalVerifyCommand(ConfigurationManager configurationManager, ArtifactRepository artifactRepository) {
        super(configurationManager, artifactRepository);
    }

    public ResponseEntity<?> verify(String storageId, String repositoryId, String oid) {
        String oidPath = GitLfsHelper.getOidPath(oid);
        ResponseEntity<?> response = verifyCanDownload(oid, storageId,repositoryId, oidPath, true);
        if (response == null) {
            response = ResponseEntity.status(200).build();
        }
        return response;
    }
}
