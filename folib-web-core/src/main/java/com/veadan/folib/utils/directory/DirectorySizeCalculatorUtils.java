package com.veadan.folib.utils.directory;

import cn.hutool.extra.spring.SpringUtil;
import com.veadan.folib.providers.io.RepositoryFiles;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.providers.io.RepositoryPathResolver;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;

import java.io.IOException;
import java.nio.file.FileVisitResult;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.SimpleFileVisitor;
import java.nio.file.attribute.BasicFileAttributes;

/**
 * 目录大小计算工具类
 *
 * @author leipenghui
 * @date 2024/7/18
 **/
@Slf4j
public class DirectorySizeCalculatorUtils {

    /**
     * 目录
     */
    private final RepositoryPath directory;

    public DirectorySizeCalculatorUtils(RepositoryPath directory) {
        this.directory = directory;
    }

    public Result compute() {
        final Result result = new Result();
        try {
            if (!Files.exists(directory)) {
                log.warn("DirectorySizeCalculatorUtils directory [{}] not exists", directory);
                return result;
            }
            RepositoryPathResolver repositoryPathResolver = SpringUtil.getBean(RepositoryPathResolver.class);
            String storageId = directory.getStorageId(), repositoryId = directory.getRepositoryId();
            String repositoryPrefix = String.format("/%s/%s/", storageId, repositoryId);
            Files.walkFileTree(directory.getTarget(), new SimpleFileVisitor<>() {
                @Override
                public FileVisitResult visitFile(Path file, BasicFileAttributes attrs) {
                    try {
                        String path = file.toString(), name = file.getFileName().toString();
                        if (exclude(name)) {
                            log.warn("Visit file path [{}] skip...", file);
                            return FileVisitResult.CONTINUE;
                        }
                        String artifactPath = path.substring(path.indexOf(repositoryPrefix) + repositoryPrefix.length());
                        RepositoryPath repositoryPath = repositoryPathResolver.resolve(storageId, repositoryId, artifactPath);
                        boolean isTrash = RepositoryFiles.isTrash(repositoryPath);
                        if (isTrash) {
                            result.incrementTrashFilesCount();
                            result.addToTrashFilesSize(attrs.size());
                            if (RepositoryFiles.isArtifact(repositoryPath)) {
                                result.incrementTrashArtifactsCount();
                                result.addToTrashArtifactsSize(attrs.size());
                            }
                            return FileVisitResult.CONTINUE;
                        }
                        result.incrementFilesCount();
                        result.addToFilesSize(attrs.size());
                        log.debug("Pre visit directory [{}] path [{}]", directory, path);
                        if (RepositoryFiles.isArtifact(repositoryPath)) {
                            result.incrementArtifactsCount();
                            result.addToArtifactsSize(attrs.size());
                        }
                    } catch (Exception ex) {
                        log.error("DirectorySizeCalculatorUtils path [{}] error [{}]", file, ExceptionUtils.getStackTrace(ex));
                    }
                    return FileVisitResult.CONTINUE;
                }

                @Override
                public FileVisitResult preVisitDirectory(Path dir, BasicFileAttributes attrs) {
                    try {
                        if (Files.isSameFile(directory, dir)) {
                            result.incrementDirectoriesCount();
                            return FileVisitResult.CONTINUE;
                        }
                        log.debug("Pre visit directory [{}] dir [{}]", directory, dir);
                        String path = dir.toString();
                        String artifactPath = path.substring(path.indexOf(repositoryPrefix) + repositoryPrefix.length());
                        RepositoryPath repositoryPath = repositoryPathResolver.resolve(storageId, repositoryId, artifactPath);
                        boolean isTrash = RepositoryFiles.isTrash(repositoryPath);
                        if (isTrash) {
                            result.incrementTrashDirectoriesCount();
                            return FileVisitResult.CONTINUE;
                        }
                        result.incrementDirectoriesCount();
                    } catch (Exception ex) {
                        log.error("DirectorySizeCalculatorUtils dir [{}] error [{}]", dir, ExceptionUtils.getStackTrace(ex));
                    }
                    return FileVisitResult.CONTINUE;
                }
            });
        } catch (IOException e) {
            log.error("DirectorySizeCalculatorUtils directory [{}] error [{}]", directory, ExceptionUtils.getStackTrace(e));
        }
        return result;
    }

    public static boolean exclude(String name) {
        if (StringUtils.isBlank(name)) {
            return true;
        }
        String dsStore = ".DS_Store";
        if (name.endsWith(dsStore)) {
            return true;
        }
        return false;
    }

}

