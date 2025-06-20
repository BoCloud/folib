package com.veadan.folib.storage.manager;

import com.veadan.folib.constant.GlobalConstants;
import com.veadan.folib.providers.io.RepositoryFiles;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.storage.repository.Repository;
import org.apache.commons.collections4.MapUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

import java.io.IOException;
import java.nio.file.Files;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

/**
 * @author Kate Novik.
 */
@Component
public class RawArtifactManager {


    private static final Logger logger = LoggerFactory.getLogger(RawArtifactManager.class);


    public RawArtifactManager() {
    }

    public void deleteArtifacts(RepositoryPath basePath, LinkedHashMap<RepositoryPath, List<RepositoryPath>> visitedRootPaths, int numberToKeep, Map<String, String> cleanupArtifactPathMap)
            throws IOException {
        Repository repository = basePath.getRepository();
        if (!Files.exists(basePath)) {
            logger.warn("Removal of Raw artifact: {} not exists.", basePath);
            return;
        }
        logger.info("Removal of Raw artifact {} in '{}:{}'.",
                basePath, repository.getStorage().getId(), repository.getId());
        for (Map.Entry<RepositoryPath, List<RepositoryPath>> entry : visitedRootPaths.entrySet()) {
            RepositoryPath repositoryPath = entry.getKey();
            try {
                List<RepositoryPath> repositoryPathList = entry.getValue();
                int size = repositoryPathList.size();
                int artifactNumberToKeep = getNumberToKeep(RepositoryFiles.relativizePath(repositoryPath), numberToKeep, cleanupArtifactPathMap);
                logger.info("Remove raw artifact job storageId [{}] repositoryId [{}] path [{}] version count [{}] numberToKeep [{}] versions [{}]", basePath.getStorageId(), basePath.getRepositoryId(), RepositoryFiles.relativizePath(repositoryPath), size, artifactNumberToKeep, repositoryPathList);
                if (artifactNumberToKeep > 0 && size > 1 && size > artifactNumberToKeep) {
                    List<RepositoryPath> versions = repositoryPathList.subList(0, size - artifactNumberToKeep);
                    logger.info("Remove raw artifact job storageId [{}] repositoryId [{}] path [{}] version count [{}] numberToKeep [{}] delete versions [{}]", basePath.getStorageId(), basePath.getRepositoryId(), RepositoryFiles.relativizePath(repositoryPath), size, artifactNumberToKeep, versions);
                    for (RepositoryPath deleteRepositoryPath : versions) {
                        handlerRepositoryPath(deleteRepositoryPath, repository.isAllowsForceDeletion());
                    }
                }
            } catch (Exception ex) {
                logger.error("Remove raw artifact job storageId [{}] repositoryId [{}] path [{}] error [{}]", basePath.getStorageId(), basePath.getRepositoryId(), repositoryPath, ExceptionUtils.getStackTrace(ex));
            }
        }
    }

    private void handlerRepositoryPath(RepositoryPath deleteRepositoryPath, boolean force) {
        try {
            if (!Files.exists(deleteRepositoryPath)) {
                return;
            }
            RepositoryFiles.delete(deleteRepositoryPath, force);
            logger.info("Remove raw artifact job storageId [{}] repositoryId [{}] path [{}] force [{}]", deleteRepositoryPath.getStorageId(), deleteRepositoryPath.getRepositoryId(), deleteRepositoryPath, force);
        } catch (Exception ex) {
            logger.error("Remove raw artifact job storageId [{}] repositoryId [{}] path [{}] force [{}] error [{}]", deleteRepositoryPath.getStorageId(), deleteRepositoryPath.getRepositoryId(), deleteRepositoryPath, force, ExceptionUtils.getStackTrace(ex));
        }
    }

    private int getNumberToKeep(String artifactPath, int numberToKeep, Map<String, String> cleanupArtifactPathMap) {
        if (MapUtils.isEmpty(cleanupArtifactPathMap)) {
            return numberToKeep;
        }
        String cleanupArtifactPath, cleanupArtifactPathValue, cleanupArtifactPathPrefix;
        for (Map.Entry<String, String> entry : cleanupArtifactPathMap.entrySet()) {
            cleanupArtifactPath = entry.getKey();
            cleanupArtifactPathValue = entry.getValue();
            if (StringUtils.isBlank(cleanupArtifactPath) || StringUtils.isBlank(cleanupArtifactPathValue)) {
                continue;
            }
            //获取目录、制品级别生命周期，优先级第一
            cleanupArtifactPathPrefix = cleanupArtifactPath + GlobalConstants.SEPARATOR;
            if (artifactPath.equals(cleanupArtifactPath) || artifactPath.startsWith(cleanupArtifactPathPrefix) || artifactPath.matches(cleanupArtifactPath)) {
                return Integer.parseInt(entry.getValue());
            }
        }
        //仓库级别生命周期，优先级最低
        return numberToKeep;
    }
}
