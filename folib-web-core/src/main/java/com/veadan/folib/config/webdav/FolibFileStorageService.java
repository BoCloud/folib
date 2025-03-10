package com.veadan.folib.config.webdav;

import com.google.common.base.Joiner;
import com.veadan.folib.domain.ArtifactPromotion;
import com.veadan.folib.domain.DirectoryListing;
import com.veadan.folib.dto.TargetRepositoyDto;
import com.veadan.folib.promotion.ArtifactUploadTask;
import com.veadan.folib.providers.io.LayoutFileSystem;
import com.veadan.folib.providers.io.RepositoryFiles;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.providers.io.RepositoryPathResolver;
import com.veadan.folib.services.ArtifactManagementService;
import com.veadan.folib.services.ArtifactPromotionService;
import com.veadan.folib.services.ConfigurationManagementService;
import com.veadan.folib.services.DirectoryListingService;
import com.veadan.folib.services.RepositoryManagementService;
import com.veadan.folib.storage.Storage;
import com.veadan.folib.storage.repository.Repository;
import com.veadan.folib.storage.repository.RepositoryTypeEnum;

import com.veadan.folib.utils.PathUtils;
import io.milton.http.exceptions.BadRequestException;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.mock.web.MockMultipartFile;
import org.springframework.stereotype.Service;

import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.attribute.FileTime;
import java.util.Arrays;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

/**
 * @author huayanjun
 * @since 2025-03-09 16:14
 */
@Slf4j
@Service
public class FolibFileStorageService implements FileStorageService {

    @Autowired
    private RepositoryManagementService repositoryManagementService;

    @Autowired
    private RepositoryPathResolver repositoryPathResolver;

    @Autowired
    private ConfigurationManagementService configurationManagementService;

    @Autowired
    @Qualifier("browseRepositoryDirectoryListingService")
    private volatile DirectoryListingService directoryListingService;

    @Autowired
    private ArtifactPromotionService artifactPromotionService;

    @Autowired
    private ArtifactManagementService artifactManagementService;

    @Autowired
    private ConfigurationManagementService configurationManager;

    @Value("${folib.temp}")
    private String tempPath;


    @Override
    public byte[] getFileContent(String path) throws IOException {
        log.info("path:{}", path);
        RepositoryPath repositoryPath = getRepositoryPath(path);
        if (repositoryPath == null) {
            return null;
        }
        try (InputStream inputStream = Files.newInputStream(repositoryPath)) {
            return inputStream.readAllBytes();
        }
    }

    @Override
    public void saveFileContent(String path, byte[] content) throws IOException {
        RepositoryPath repositoryPath = getRepositoryPath(path);
        if (repositoryPath == null) {
            throw new IllegalArgumentException("path:" + path + " is not a valid repository path");
        }
        String repoPath = getRepoPath(path);
        String name= PathUtils.getLastPathElement(path);
        ArtifactUploadTask artifactUploadTask = new ArtifactUploadTask(repositoryPath.getStorageId(), repositoryPath.getRepositoryId(), new MockMultipartFile(name,content), repoPath, tempPath);
        artifactUploadTask.call();
    }

    @Override
    public DirectoryListing listDirectory(String path) throws IOException {
        String[] split = path.split("/");
        if (split.length == 0 || (split.length == 1 && split[0].isEmpty())) {
            Map<String, Storage> storages = configurationManager.getConfiguration().getStorages();
            return directoryListingService.fromStorages(storages);
        }
        String storageId = split[0];
        Storage storage = repositoryManagementService.getStorage(storageId);
        if (storage == null) {
            return new DirectoryListing();
        }
        if (split.length == 1) {
            // 只显示本地仓库
            Map<String,Repository> collect = storage.getRepositories().values().stream().filter(repository ->
                    RepositoryTypeEnum.HOSTED.getType().equals(repository.getType())
            ).collect(Collectors.toMap(Repository::getId, repository -> repository));
            return directoryListingService.fromRepositories(collect);
        } else {
            String repositoryId = split[1];
            Repository repository = storage.getRepository(repositoryId);
            String[] repo = Arrays.copyOfRange(split, 2, split.length);
            String repoPath = Joiner.on("/").join(repo);
            RepositoryPath repositoryPath = repositoryPathResolver.resolve(repository, repoPath);
            try {
                if (repositoryPath == null || !Files.exists(repositoryPath)) {
                    return null;
                }
                if (!repository.isInService()) {
                    return null;
                }
                if (!repository.isAllowsDirectoryBrowsing() || !probeForDirectoryListing(repositoryPath)) {
                    return null;
                }
                return directoryListingService.fromRepositoryPath(repositoryPath);

            } catch (Exception e) {
                log.error(e.getMessage(), e);
            }
        }
        return null;
    }

    @Override
    public void copyFile(String sourcePath, String destPath) throws BadRequestException {
        ArtifactPromotion artifactPromotion = getArtifactPromotion(sourcePath, destPath);
        artifactPromotionService.copy(artifactPromotion);
    }

    @Override
    public void moveFile(String sourcePath, String destPath) throws BadRequestException {
        ArtifactPromotion artifactPromotion = getArtifactPromotion(sourcePath, destPath);
        artifactPromotionService.move(artifactPromotion);
    }

    @Override
    public void deleteFile(String path) throws IOException {
        RepositoryPath repositoryPath = getRepositoryPath(path);
        if (repositoryPath == null || !Files.exists(repositoryPath)) {
            log.warn("Cannot delete file from {} because it does not exist", path);
            return;
        }
        Files.delete(repositoryPath);

    }


    @Override
    public boolean isDirectory(String path) {
        log.info("判断路径是否为目录:{}", path);
        String[] split = path.split("/");
        if (split.length < 3) {
            return true;
        } else {
            String storageId = split[0];
            Storage storage = repositoryManagementService.getStorage(storageId);
            String repositoryId = split[1];
            Repository repository = storage.getRepository(repositoryId);
            String[] repo = Arrays.copyOfRange(split, 2, split.length);
            String repoPath = Joiner.on("/").join(repo);
            RepositoryPath repositoryPath = repositoryPathResolver.resolve(repository, repoPath);
            boolean directory = Files.isDirectory(repositoryPath);
            log.info("该路径是否为目录:{}", directory);
            return directory;
        }
    }

    @Override
    public boolean exists(String path) {
        String[] split = path.split("/");
        log.info("path长度是:{}", split.length);

        // 如果路径为空或根路径，返回true
        if (split.length == 0 || (split.length == 1 && split[0].isEmpty())) {
            return true;
        }
        String storageId = split[0];
        Storage storage = repositoryManagementService.getStorage(storageId);
        Repository repository;
        // 如果storage不存在，返回false
        if (storage == null) {
            return false;
        }
        // 如果路径只有一级，检查storage是否存在
        if (split.length == 1) {
            return true;
        }
        // 检查路径大于1级，检查repository是否存在
        String repositoryId = split[1];
        repository = storage.getRepository(repositoryId);
        if (repository == null) {
            return false;
        }
        String[] repo = Arrays.copyOfRange(split, 2, split.length);
        String repoPath = Joiner.on("/").join(repo);
        RepositoryPath repositoryPath = repositoryPathResolver.resolve(repository, repoPath);
        return Files.exists(repositoryPath);
    }

    @Override
    public Long getContentLength(String path) {
        RepositoryPath repositoryPath = getRepositoryPath(path);
        try {
            return Files.size(repositoryPath);
        } catch (IOException e) {
            return 0L;
        }

    }

    protected boolean probeForDirectoryListing(final RepositoryPath repositoryPath)
            throws IOException {
        return Files.exists(repositoryPath) &&
                repositoryPath.getRepository().getLayout().equals("helm") && repositoryPath.getTarget().toString().endsWith("index.yaml") || Files.isDirectory(repositoryPath) &&
                isPermittedForDirectoryListing(repositoryPath);
    }

    protected boolean isPermittedForDirectoryListing(final RepositoryPath repositoryPath)
            throws IOException {
        return (!Files.isHidden(repositoryPath)
                // 支持Cocoapods索引目录的显示
                || repositoryPath.toString().contains(".specs") || repositoryPath.toString().contains(LayoutFileSystem.TRASH))
                && !RepositoryFiles.isTemp(repositoryPath);
    }


    private RepositoryPath getRepositoryPath(String path) {
        String[] split = path.split("/");
        if (split.length < 2) {
            return null;
        }
        String storageId = split[0];
        Storage storage = repositoryManagementService.getStorage(storageId);
        if (storage == null) {
            return null;
        }
        String repositoryId = split[1];
        Repository repository = storage.getRepository(repositoryId);
        if (repository == null) {
            return null;
        }
        String[] repo = Arrays.copyOfRange(split, 2, split.length);
        String relativePath = Joiner.on("/").join(repo);
        return repositoryPathResolver.resolve(repository, relativePath);
    }

    private String getRepoPath(String path) {
        String[] split = path.split("/");
        String[] repo = Arrays.copyOfRange(split, 2, split.length);
        return Joiner.on("/").join(repo);
    }

    private ArtifactPromotion getArtifactPromotion(String sourcePath, String targetPath) throws BadRequestException {
        ArtifactPromotion artifactPromotion = new ArtifactPromotion();
        RepositoryPath sourceRepoPath = getRepositoryPath(sourcePath);
        RepositoryPath targetRepoPath = getRepositoryPath(targetPath);
        if (sourceRepoPath == null || targetRepoPath == null) {
            throw new BadRequestException("无效的地址");
        }

        artifactPromotion.setSrcStorageId(sourceRepoPath.getStorageId());
        artifactPromotion.setSrcRepositoryId(sourceRepoPath.getRepositoryId());
        artifactPromotion.setPath(getRepoPath(sourcePath));

        TargetRepositoyDto targetRepositoyDto = new TargetRepositoyDto();
        targetRepositoyDto.setTargetStorageId(targetRepoPath.getStorageId());
        targetRepositoyDto.setTargetRepositoryId(targetRepoPath.getRepositoryId());
        artifactPromotion.setTargetRepositoyList(List.of(targetRepositoyDto));
        artifactPromotion.setTargetPath(getRepoPath(targetPath));
        return artifactPromotion;
    }

    @Override
    public Date getModifiedDate(String path) {
        RepositoryPath repositoryPath = getRepositoryPath(path);
        try {
            FileTime lastModifiedTime = Files.getLastModifiedTime(repositoryPath);
            return new Date(lastModifiedTime.toMillis());
        } catch (Exception e) {
            return new Date();
        }
    }

    @Override
    public void createDirectory(String path) throws IOException {
        RepositoryPath repositoryPath = getRepositoryPath(path);
        Files.createDirectory(repositoryPath);

    }


    public void saveFileContentFromStream(String path, InputStream inputStream, Long length) throws IOException {
        if (exists(path)) {
            throw new IllegalArgumentException("File or directory already exists: " + path);
        }
        byte[] content = length != null ?
                inputStream.readNBytes(length.intValue()) :
                inputStream.readAllBytes();
        saveFileContent(path, content);
    }


}


