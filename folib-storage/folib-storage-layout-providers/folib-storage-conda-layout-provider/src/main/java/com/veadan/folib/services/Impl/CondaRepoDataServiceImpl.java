package com.veadan.folib.services.Impl;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.veadan.folib.index.indexer.CondaMetadataExtractor;
import com.veadan.folib.index.indexer.CondaMetadataIndexer;
import com.veadan.folib.index.model.RepoData;
import com.veadan.folib.index.model.RepoDataEventKind;
import com.veadan.folib.index.model.RepoDataPackage;
import com.veadan.folib.services.CondaRepoDataService;
import lombok.NonNull;
import org.springframework.stereotype.Service;

import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.Path;

/**
 * @author LingengMa
 * @date 2025/04/14 14:01
 * @Description:
 */

@Service
public class CondaRepoDataServiceImpl implements CondaRepoDataService {
    CondaMetadataExtractor condaMetadataExtractor = new CondaMetadataExtractor();

    CondaMetadataIndexer condaMetadataIndexer = new CondaMetadataIndexer(condaMetadataExtractor);

    @Override
    public RepoData getRepoData(String repoKey) {
        return this.getRepoDataFromPath(getRepoDataPath(repoKey));
    }

    @Override
    public RepoData getCurrentRepoData(String repoKey) {
        return this.getRepoDataFromPath(getCurrentRepoDataPath(repoKey));
    }

    @Override
    public void sendRepoDataEvent(RepoDataEventKind kind, String repoKey, String artifactName) {
        RepoData repoData = getRepoData(repoKey);
        RepoData currentRepoData = getCurrentRepoData(repoKey);
        if (kind == RepoDataEventKind.ADD) {
            addPackage(repoData, currentRepoData, repoKey, artifactName);
        } else if (kind == RepoDataEventKind.REMOVE) {
            removePackage(repoData, currentRepoData, repoKey, artifactName);
        } else if (kind == RepoDataEventKind.REINDEX) {
            reindexPackage(repoData, repoKey);
        }
        saveRepoData(repoData, getRepoDataPath(repoKey));
        saveRepoData(currentRepoData, getCurrentRepoDataPath(repoKey));
    }

    @Override
    public boolean checkPackageExistsInRepoData(RepoData repoData,
                                                String artifactName) {
        return condaMetadataIndexer.checkPackageExistsInRepoData(repoData, artifactName);
    }

    private void addPackage(RepoData repoData, RepoData currentRepoData, String repoKey, String artifactName) {
        if (this.checkPackageExistsInRepoData(repoData, artifactName)) {
            return;
        }

        RepoDataPackage repoDataPackage = condaMetadataIndexer.getRepoDataPackage(repoKey, artifactName);
        if (repoDataPackage == null) {
            throw new RuntimeException("Failed to get RepoDataPackage for artifact: " + artifactName);
        }
        condaMetadataIndexer.addPackageToRepodata(repoData, repoDataPackage, artifactName);
        condaMetadataIndexer.addPackageToCurrentRepoData(currentRepoData, repoData, repoDataPackage, artifactName);
    }

    /**
     * Remove package from repodata
     *
     * @param repoData
     * @param repoKey
     * @param artifactName
     * @return
     */
    private void removePackage(RepoData repoData, RepoData currentRepoData, String repoKey, String artifactName) {
        if (!this.checkPackageExistsInRepoData(repoData, artifactName)) {
            return;
        }
        condaMetadataIndexer.removePackageFromRepodata(repoData, artifactName);
        condaMetadataIndexer.reindexCurrentRepoData(repoData, repoKey);
    }

    private void reindexPackage(RepoData repoData, String repoKey) {
    }

    private void saveRepoData(RepoData repoData, String repoDataPath) {
        try {
            Path path = Path.of(repoDataPath);
            // 检查父目录是否存在，不存在则创建
            if (!Files.exists(path.getParent())) {
                Files.createDirectories(path.getParent());
            }
            Files.writeString(path, repoData.toJsonPretty());
        } catch (IOException e) {
            throw new RuntimeException("Failed to save RepoData to path: " + repoDataPath, e);
        }
    }


    @NonNull
    private RepoData getRepoDataFromPath(@NonNull String repoDataPath) {
        // 1. 检查repoDataPath是否存在
        if (!Files.exists(Path.of(repoDataPath))) {
            Path path = Path.of(repoDataPath);
            return condaMetadataIndexer.createNewRepoData(path.getParent().toString());
        }
        // 2. 获取索引数据
        ObjectMapper objectMapper = new ObjectMapper();
        try (InputStream inputStream = Files.newInputStream(Path.of(repoDataPath))) {
            return objectMapper.readValue(inputStream, RepoData.class);
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    private String getRepoDataPath(String repoKey) {
        return repoKey + "/repodata.json";
    }

    private String getCurrentRepoDataPath(String repoKey) {
        return repoKey + "/current_repodata.json";
    }

}
