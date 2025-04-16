package com.veadan.folib.services.Impl;

import com.veadan.folib.index.model.RepoData;
import com.veadan.folib.index.model.RepoDataEventKind;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.services.ArtifactManagementService;
import com.veadan.folib.services.CondaArtifactService;
import com.veadan.folib.services.CondaRepoDataService;
import lombok.NonNull;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.nio.file.Files;

/**
 * @author LingengMa
 * @date 2025/04/16 10:24
 * @Description: 
 */

@Service
public class CondaArtifactServiceImpl implements CondaArtifactService {
    private final CondaRepoDataService condaRepoDataService;

    private final ArtifactManagementService artifactManagementService;

    @Autowired
    public CondaArtifactServiceImpl(CondaRepoDataService condaRepoDataService, ArtifactManagementService artifactManagementService) {
        this.condaRepoDataService = condaRepoDataService;
        this.artifactManagementService = artifactManagementService;
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
        String fileName = path.getFileName().toString();

        // 2. 检查文件是否存在
        fileExist = Files.exists(path);

        // 3. 检查索引文件是否存在, 且文件是否在索引中
        RepoData repoData = condaRepoDataService.getRepoData(parentPath);
        indexExist = condaRepoDataService.checkPackageExistsInRepoData(repoData, fileName);

        if (fileExist && indexExist) {
            return true;
        } else if (!fileExist && !indexExist) {
            return false;
        } else if (fileExist && !indexExist) {
            // 4. 文件存在, 索引不存在, 则添加索引
            condaRepoDataService.sendRepoDataEvent(RepoDataEventKind.ADD, parentPath, fileName);
            return true;
        } else if (!fileExist && indexExist) {
            // 5. 文件不存在, 索引存在, 则删除索引
            condaRepoDataService.sendRepoDataEvent(RepoDataEventKind.REMOVE, parentPath, fileName);
            return false;
        }
        return false;
    }

    /**
     * @Description: 删除包
     * @param path: 完整的文件路径, repoKey/artifactName
     * @return
     */
    public void unpublishPackage(@NonNull RepositoryPath path) throws Exception {
        String parentPath = path.getParent().toString();
        String fileName = path.getFileName().toString();
        if (!checkArtifactExist(path)) {
            throw new Exception("File does not exist: " + path);
        }

        condaRepoDataService.sendRepoDataEvent(RepoDataEventKind.REMOVE, parentPath, fileName);
        try {
            artifactManagementService.delete(path, false);
        } catch (Exception e) {
            throw new Exception("Failed to delete file: " + path, e);
        }
    }


}
