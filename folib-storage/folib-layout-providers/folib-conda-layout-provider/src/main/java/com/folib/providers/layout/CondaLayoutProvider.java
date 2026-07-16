package com.folib.providers.layout;

import com.folib.artifact.coordinates.CondaCoordinates;
import com.folib.configuration.ConfigurationManager;
import com.folib.index.cache.CondaIndexCache;
import com.folib.providers.io.RepositoryFileAttributeType;
import com.folib.providers.io.RepositoryFiles;
import com.folib.providers.io.RepositoryPath;
import com.folib.providers.io.RepositoryPathResolver;
import com.folib.repository.CondaRepositoryFeatures;
import com.folib.repository.CondaRepositoryStrategy;
import com.folib.services.CondaRepoDataService;
import com.folib.storage.Storage;
import com.folib.storage.repository.Repository;
import com.folib.storage.repository.RepositoryTypeEnum;
import org.apache.commons.lang3.BooleanUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

import jakarta.annotation.PostConstruct;
import javax.inject.Inject;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Instant;
import java.time.temporal.ChronoUnit;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.stream.Collectors;


@Component("condaLayoutProvider")
public class CondaLayoutProvider extends AbstractLayoutProvider<CondaCoordinates> {

    private static final Logger logger = LoggerFactory.getLogger(CondaLayoutProvider.class);

    public static final String ALIAS = CondaCoordinates.LAYOUT_NAME;

    @Inject
    private CondaRepositoryStrategy condaRepositoryManagementStrategy;

    @Inject
    private ConfigurationManager configurationManager;

    @Inject
    private CondaRepositoryFeatures condaRepositoryFeatures;

    @Inject
    private CondaRepoDataService condaRepoDataService;

    @Inject
    private CondaIndexCache condaIndexCache;

    @Inject
    private RepositoryPathResolver repositoryPathResolver;

    @PostConstruct
    public void register() {
        logger.info("Registered layout provider '{}' with alias '{}'.", getClass().getCanonicalName(), ALIAS);
    }

    @Override
    public CondaCoordinates getArtifactCoordinates(RepositoryPath path) throws IOException {

        return CondaCoordinates.parse(RepositoryFiles.relativizePath(path));
    }

    @Override
    public boolean isArtifactMetadata(RepositoryPath path) {
        return path.getFileName().toString().endsWith("index.json");
    }

    public boolean isCondaRepoData(RepositoryPath path) {
        return path.getFileName().toString().endsWith("repodata.json") ||
               path.getFileName().toString().endsWith("current_repodata.json");
    }


    @Override
    protected Map<RepositoryFileAttributeType, Object> getRepositoryFileAttributes(RepositoryPath repositoryPath,
                                                                                   RepositoryFileAttributeType... attributeTypes)
            throws IOException {
        if (attributeTypes == null || attributeTypes.length == 0) {
            return super.getRepositoryFileAttributes(repositoryPath, attributeTypes);
        }

        Map<RepositoryFileAttributeType, Object> result = new ConcurrentHashMap<>(super.getRepositoryFileAttributes(repositoryPath, attributeTypes));

        for (RepositoryFileAttributeType attributeType : attributeTypes) {
            Object value = result.get(attributeType);

            switch (attributeType) {
                case REFRESH_CONTENT:
                    try {
                        if (value instanceof Boolean) {
                            Instant halfAnHourAgo = Instant.now().minus(refreshContentInterval(repositoryPath), ChronoUnit.MINUTES);
                            boolean refreshContentValue = BooleanUtils.isTrue((Boolean) value) ||
                                    (!RepositoryTypeEnum.HOSTED.getType().equals(repositoryPath.getRepository().getType()) && isIndex(repositoryPath)) &&
                                            !RepositoryFiles.wasModifiedAfter(repositoryPath, halfAnHourAgo);

                            if (refreshContentValue) {
                                condaIndexCache.reset(repositoryPath.toString());
                            }
                            result.put(attributeType, refreshContentValue);
                        }
                    } catch (Exception e) {
                        // Log the exception or handle it appropriately
                        logger.error("Error processing REFRESH_CONTENT attribute", e);
                        throw new IOException("Error processing REFRESH_CONTENT attribute", e);
                    }
                    break;
                default:
                    break;
            }
        }

        return result;
    }


    private boolean isIndex(RepositoryPath repositoryPath) {
        if (repositoryPath == null || repositoryPath.getPath() == null || repositoryPath.getPath().isEmpty()) {
            return false;
        }
        String path = repositoryPath.getPath();
        return path.endsWith("repodata.json") || path.endsWith("current_repodata.json");
    }

    @Override
    public CondaRepositoryStrategy getRepositoryManagementStrategy() {
        return condaRepositoryManagementStrategy;
    }

    @Override
    public Set<String> getDefaultArtifactCoordinateValidators() {
        return condaRepositoryFeatures.getDefaultArtifactCoordinateValidators();
    }

    @Override
    public String getAlias() {
        return ALIAS;
    }

    @Override
    public void initData(String storageId, String repositoryId) {
        logger.info(" rpm repository initData storageId:{} repositoryId:{}", storageId,repositoryId);
        // 获取存储配置时添加空指针检查
        Storage storage = configurationManager.getConfiguration().getStorage(storageId);
        if (storage == null) {
            throw new IllegalStateException("Storage not found: " + storageId);
        }

        // 获取仓库时添加空指针检查
        Repository repository = storage.getRepository(repositoryId);
        if (repository == null) {
            throw new IllegalStateException("Repository not found: " + repositoryId);
        }

        // 提前返回条件判断保持原逻辑
        if (!"group".equals(repository.getType())) {
            return;
        }

        RepositoryPath repositoryPath = repositoryPathResolver.resolve(repository);
        try {
            // 获取其下所有子目录(不包括当前目录)
            Set<Path> subDirectories = Files.walk(repositoryPath, 1)
                    .filter(Files::isDirectory)
                    .collect(Collectors.toSet());
            subDirectories.remove(repositoryPath);

            for (Path subDirectory : subDirectories) {
                // 过滤掉以.开头的目录
                String platformId = subDirectory.getFileName().toString();
                if (platformId.startsWith(".")) {
                    continue;
                }
                Path repoDataPath = subDirectory.resolve("repodata.json");
                // 删除 repodata.json 文件
                Files.deleteIfExists(repoDataPath);
                // 重新构建, 并传播
                condaRepoDataService.getRepoData(repository, platformId);
            }
        } catch (IOException e) {
            logger.error("Error while walking through the directory: {}", repositoryPath, e);
        }
    }
}
