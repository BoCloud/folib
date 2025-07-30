package com.folib.services.Impl;

import com.folib.event.CondaRepodataEvent;
import com.folib.index.indexer.CondaMetadataExtractor;
import com.folib.index.indexer.CondaMetadataIndexer;
import com.folib.index.model.Index;
import com.folib.index.model.RepoData;
import com.folib.index.model.RepoDataEventKind;
import com.folib.index.model.RepoDataPackage;
import com.folib.providers.io.RepositoryPath;
import com.folib.services.ArtifactManagementService;
import com.folib.services.CondaArtifactService;
import com.folib.services.CondaRepoDataService;
import com.folib.storage.repository.Repository;
import lombok.NonNull;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.nio.file.Files;


@Service
public class CondaArtifactServiceImpl implements CondaArtifactService {
    private final CondaRepoDataService condaRepoDataService;

    private final ArtifactManagementService artifactManagementService;

    private final CondaMetadataExtractor condaMetadataExtractor;

    private final CondaMetadataIndexer condaMetadataIndexer;

    @Autowired
    public CondaArtifactServiceImpl(CondaRepoDataService condaRepoDataService, ArtifactManagementService artifactManagementService, CondaMetadataExtractor condaMetadataExtractor, CondaMetadataIndexer condaMetadataIndexer) {
        this.condaRepoDataService = condaRepoDataService;
        this.artifactManagementService = artifactManagementService;
        this.condaMetadataExtractor = condaMetadataExtractor;
        this.condaMetadataIndexer = condaMetadataIndexer;
    }

    /**
     * @Description: 检查包是否存在, FileExist and IndexExist. 若不一致, 则修复(删除索引或添加索引)
     * @param path: 完整的文件路径, repoKey/artifactName
     * @return
     */
    public boolean checkArtifactExist(@NonNull RepositoryPath path) throws Exception {
        boolean fileExist = false;

        boolean indexExist = false;
        // 1. 提取父目录和文件名和indexPath
        String parentPath = path.getParent().toString();
        String artifactName = path.getFileName().toString();
        String platformId = parentPath.substring(parentPath.lastIndexOf("/") + 1);

        // 2. 检查文件是否存在
        fileExist = Files.exists(path);

        // 3. 检查索引文件是否存在, 且文件是否在索引中
        RepoData repoData = condaRepoDataService.getRepoData(path.getRepository(), platformId);
        indexExist = condaRepoDataService.checkPackageExistsInRepoData(repoData, artifactName);
        // 4. 提取platformId
        Repository repository = path.getRepository();

        if (fileExist && indexExist) {
            return true;
        } else if (!fileExist && !indexExist) {
            return false;
        } else if (fileExist && !indexExist) {
            // 5. 文件存在, 索引不存在, 则添加索引
            RepoDataPackage repoDataPackage = getRepoDataPackage(path);
            condaRepoDataService.sendRepoDataEvent(
                    new CondaRepodataEvent(RepoDataEventKind.ADD, repository, platformId, artifactName, repoDataPackage)
            );
            return true;
        } else if (!fileExist && indexExist) {
            // 6. 文件不存在, 索引存在, 则删除索引
            condaRepoDataService.sendRepoDataEvent(
                    new CondaRepodataEvent(RepoDataEventKind.REMOVE, repository,platformId, artifactName)
            );
            return false;
        }
        return false;
    }

    /**
     * @Description: 重新将某个包添加到索引中/删除
     * @param path
     * @throws Exception
     */
    @Override
    public void reIndexArtifact(@NonNull RepositoryPath path) throws Exception {
        boolean fileExist = false;
        // 1. 提取父目录和文件名和indexPath
        String repoKey = path.getParent().toString();
        String artifactName = path.getFileName().toString();

        // 2. 检查文件是否存在
        fileExist = Files.exists(path);

        // 提取platformId
        Repository repository = path.getRepository();
        String platformId = repoKey.substring(repoKey.lastIndexOf("/") + 1);

        condaRepoDataService.sendRepoDataEvent(
                new CondaRepodataEvent(RepoDataEventKind.REMOVE, repository, platformId, artifactName)
        );
        if (fileExist) {   // 文件存在, 则添加索引
            RepoDataPackage repoDataPackage = getRepoDataPackage(path);
            condaRepoDataService.sendRepoDataEvent(
                    new CondaRepodataEvent(RepoDataEventKind.ADD, repository, platformId, artifactName, repoDataPackage)
            );
        }
        return;
    }

    /**
     * @Description: 删除包
     * @param path: 完整的文件路径, repoKey/artifactName
     * @return
     */
    public void unpublishPackage(@NonNull RepositoryPath path) throws Exception {
        String parentPath = path.getParent().toString();
        String fileName = path.getFileName().toString();

        // 提取platformId
        Repository repository = path.getRepository();
        String platformId = parentPath.substring(parentPath.lastIndexOf("/") + 1);

        // 1. 检查文件是否存在
        if (!checkArtifactExist(path)) {
            throw new Exception("File does not exist: " + path);
        }
        // 2. 删除索引
        condaRepoDataService.sendRepoDataEvent(
                new CondaRepodataEvent(RepoDataEventKind.REMOVE, repository, platformId, fileName)
        );
        try {
            artifactManagementService.delete(path, false);
        } catch (Exception e) {
            throw new Exception("Failed to delete file: " + path, e);
        }
    }

    /**
     * @Description: 提取索引
     * @param repoKey: 仓库名称
     * @param artifactName: 包名称
     * @return
     */
    public Index extract(@NonNull String repoKey, @NonNull String artifactName) {
        try {
            return condaMetadataExtractor.extract(repoKey, artifactName);
        } catch (Exception e) {
            throw new RuntimeException("Failed to extract metadata for " + artifactName, e);
        }
    }


    public RepoDataPackage getRepoDataPackage(@NonNull RepositoryPath path) {
        String repoKey = path.getParent().toString();
        String artifactName = path.getFileName().toString();
        return condaMetadataIndexer.getRepoDataPackage(repoKey, artifactName);
    }

    public RepoDataPackage convertIndexToRepoDataPackage(@NonNull RepositoryPath path, @NonNull Index index) {
        String repoKey = path.getParent().toString();
        String artifactName = path.getFileName().toString();
        return condaMetadataIndexer.getRepoDataPackageWithIndex(repoKey, artifactName, index);
    }

}
