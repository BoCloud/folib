package com.veadan.folib.utils;

import cn.hutool.extra.spring.SpringUtil;
import com.veadan.folib.artifact.coordinates.DockerArtifactCoordinates;
import com.veadan.folib.configuration.ConfigurationManager;
import com.veadan.folib.domain.DockerSubsidiary;
import com.veadan.folib.providers.io.RepositoryFiles;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.storage.repository.Repository;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.compress.utils.Lists;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;

import java.io.File;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.stream.Stream;

/**
 * @author leipenghui
 * @date 2024/7/11
 **/
@Slf4j
public class DockerUtils {

    public static final String SUBSIDIARY_PATH = "subsidiaryArtifact";

    public static final String SUBSIDIARY = File.separator + SUBSIDIARY_PATH + File.separator;

    public static RepositoryPath getDockerSubsidiaryPath(RepositoryPath repositoryPath) {
        if (Files.isDirectory(repositoryPath)) {
            return repositoryPath.resolve(SUBSIDIARY_PATH);
        }
        return repositoryPath.resolveSibling(SUBSIDIARY_PATH);
    }

    public static List<DockerSubsidiary> getDockerSubsidiaryFilePaths(RepositoryPath repositoryPath) {
        List<DockerSubsidiary> dockerSubsidiaries = Lists.newArrayList();
        try {
            if (!DockerArtifactCoordinates.isDockerTag(repositoryPath)) {
                return null;
            }
            if (Files.isSameFile(repositoryPath.getRoot(), repositoryPath)) {
                return null;
            }
            RepositoryPath dockerSubsidiaryRepositoryPath = getDockerSubsidiaryPath(repositoryPath);
            if (!Files.exists(dockerSubsidiaryRepositoryPath)) {
                return null;
            }
            String repositoryBaseUrl = getRepositoryBaseUrl(repositoryPath.getRepository());
            try (Stream<Path> pathStream = Files.walk(dockerSubsidiaryRepositoryPath)) {
                pathStream
                        .forEach(p -> {
                            try {
                                RepositoryPath subRepositoryPath = (RepositoryPath) p;
                                if (!RepositoryFiles.isHidden(subRepositoryPath) && !RepositoryFiles.isChecksum(subRepositoryPath) && !RepositoryFiles.isArtifactMetadata(subRepositoryPath) && !Files.isDirectory(subRepositoryPath)) {
                                    String path = RepositoryFiles.relativizePath(subRepositoryPath);
                                    dockerSubsidiaries.add(DockerSubsidiary.builder().path(path).name(subRepositoryPath.getFileName().toString()).url(repositoryBaseUrl + path).build());
                                }
                            } catch (Exception ex) {
                                log.error("DockerSubsidiaryFilePaths error [{}]", ExceptionUtils.getStackTrace(ex));
                            }
                        });
                return dockerSubsidiaries;
            } catch (Exception ex) {
                log.error("DockerSubsidiaryFilePaths error [{}]", ExceptionUtils.getStackTrace(ex));
            }
        } catch (Exception ex) {
            log.error(ExceptionUtils.getStackTrace(ex));
        }
        return dockerSubsidiaries;
    }

    protected static String getRepositoryBaseUrl(Repository repository) {
        ConfigurationManager configurationManager = SpringUtil.getBean(ConfigurationManager.class);
        return String.format("%s/artifactory/%s/", StringUtils.chomp(configurationManager.getConfiguration().getBaseUrl(), "/"), repository.getId());
    }

    public static boolean isSubsidiaryFile(RepositoryPath path) {
        try {
            if (path.getRoot().toAbsolutePath().toString().equals(path.toAbsolutePath().toString())) {
                return false;
            }
            String parentFilename = path.getParent().getFileName().toString();
            return Objects.equals(DockerUtils.SUBSIDIARY_PATH, parentFilename);
        } catch (Exception ex) {
            log.error(ExceptionUtils.getStackTrace(ex));
        }
        return false;
    }
}
