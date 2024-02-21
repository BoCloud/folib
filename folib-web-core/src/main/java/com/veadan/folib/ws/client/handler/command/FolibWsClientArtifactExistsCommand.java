package com.veadan.folib.ws.client.handler.command;

import com.veadan.folib.domain.RepositoryPathExistCheck;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.providers.io.RepositoryPathResolver;
import com.veadan.folib.providers.layout.LayoutFileSystemProvider;
import com.veadan.folib.ws.client.context.FolibWsClientContextInfo;
import com.veadan.folib.ws.common.FolibWsAction;
import com.veadan.folib.ws.common.FolibWsSessionContextHolder;
import com.veadan.folib.ws.server.handler.command.FolibWsServerArtifactExistsResCommand;
import com.veadan.folib.ws.server.handler.command.FolibWsServerCommand;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.nio.file.Files;
import java.util.Objects;

/**
 * @author xiaodong.wang
 * @email wangxiaodong@beyondcent.com
 * @date 2023/11/10 00:38
 * @since x.x.x
 */
@Component
@Slf4j
public class FolibWsClientArtifactExistsCommand implements FolibWsServerCommand<RepositoryPathExistCheck> {

    public static final String COMMAND = "/client/artifactExists";

    @Autowired
    private RepositoryPathResolver repositoryPathResolver;

    @Override
    public String command() {
        return COMMAND;
    }

    @Override
    public void execute(RepositoryPathExistCheck repositoryPathExistCheck) {
        try {
            final RepositoryPath repositoryPath = repositoryPathResolver.resolve(repositoryPathExistCheck.getStorageId(), repositoryPathExistCheck.getRepositoryId(), repositoryPathExistCheck.getArtifactPath());
            boolean exists = false;
            String currentDigest = "";
            if (StringUtils.isNotBlank(repositoryPathExistCheck.getDigest()) && Objects.nonNull(repositoryPath) && Files.exists(repositoryPath)) {
                LayoutFileSystemProvider provider = (LayoutFileSystemProvider) repositoryPath.getFileSystem().provider();
                final RepositoryPath checksumPath = provider.getChecksumPath(repositoryPath, repositoryPathExistCheck.getDigestAlgorithm());
                if (Objects.nonNull(checksumPath) && Files.exists(checksumPath)) {
                    currentDigest = Files.readString(checksumPath);
                    exists = repositoryPathExistCheck.getDigest().equals(currentDigest);
                }
            }
            log.info("RepositoryPath [{}] [{}] [{}] digestAlgorithm [{}] digest [{}] currentDigest [{}] exists [{}]", repositoryPathExistCheck.getStorageId(), repositoryPathExistCheck.getRepositoryId(), repositoryPathExistCheck.getArtifactPath(), repositoryPathExistCheck.getDigestAlgorithm(), repositoryPathExistCheck.getDigest(), currentDigest, exists);
            final FolibWsClientContextInfo contextSessionInfo = FolibWsSessionContextHolder.getContextSessionInfo(FolibWsClientContextInfo.class);
            final String syncId = contextSessionInfo.getSyncId();
            final FolibWsAction folibWsAction = new FolibWsAction()
                    .sync(syncId)
                    .command(FolibWsServerArtifactExistsResCommand.COMMAND)
                    .payload(exists);
            contextSessionInfo.getWsRunInfo().doAction(folibWsAction);
        } catch (Exception e) {
            log.error("Handle artifactExistsCommand error", e);
        }
    }
}
