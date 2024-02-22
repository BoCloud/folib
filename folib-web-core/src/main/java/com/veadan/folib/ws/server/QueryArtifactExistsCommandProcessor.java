package com.veadan.folib.ws.server;

import com.veadan.folib.domain.RepositoryPathExistCheck;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.providers.io.RepositoryPathResolver;
import com.veadan.folib.providers.layout.LayoutFileSystemProvider;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import javax.websocket.Session;
import java.io.IOException;
import java.nio.file.Files;
import java.util.Objects;

/**
 * @author pengYongQiang
 * @date 2024/2/13 16:32
 */
@Component
@Slf4j
public class QueryArtifactExistsCommandProcessor extends CommandProcessor {

    @Autowired
    private RepositoryPathResolver repositoryPathResolver;

    @Override
    public Command getCommand() {
        return Command.QUERY_ARTIFACT_EXISTS;
    }

    @Override
    public Boolean doExecute(WSMessageRequest wsMessageRequest, Session session) {
        RepositoryPathExistCheck repositoryPathExistCheck = (RepositoryPathExistCheck) wsMessageRequest.getDate();

        final RepositoryPath repositoryPath = repositoryPathResolver.resolve(repositoryPathExistCheck.getStorageId(), repositoryPathExistCheck.getRepositoryId(), repositoryPathExistCheck.getArtifactPath());
        boolean exists = false;
        String currentDigest = "";
        if (StringUtils.isNotBlank(repositoryPathExistCheck.getDigest()) && Objects.nonNull(repositoryPath) && Files.exists(repositoryPath)) {
            LayoutFileSystemProvider provider = (LayoutFileSystemProvider) repositoryPath.getFileSystem().provider();
            final RepositoryPath checksumPath = provider.getChecksumPath(repositoryPath, repositoryPathExistCheck.getDigestAlgorithm());
            if (Objects.nonNull(checksumPath) && Files.exists(checksumPath)) {
                try {
                    currentDigest = Files.readString(checksumPath);
                } catch (IOException e) {
                    throw new RuntimeException(e);
                }
                exists = repositoryPathExistCheck.getDigest().equals(currentDigest);
            }
        }
        log.info("RepositoryPath [{}] [{}] [{}] digestAlgorithm [{}] digest [{}] currentDigest [{}] exists [{}]", repositoryPathExistCheck.getStorageId(), repositoryPathExistCheck.getRepositoryId(), repositoryPathExistCheck.getArtifactPath(), repositoryPathExistCheck.getDigestAlgorithm(), repositoryPathExistCheck.getDigest(), currentDigest, exists);
        return exists;
    }
}
