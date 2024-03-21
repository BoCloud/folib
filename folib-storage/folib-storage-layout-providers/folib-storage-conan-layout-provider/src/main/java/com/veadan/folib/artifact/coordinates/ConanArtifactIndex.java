package com.veadan.folib.artifact.coordinates;

import com.veadan.folib.constant.GlobalConstants;
import com.veadan.folib.providers.io.RepositoryFiles;
import com.veadan.folib.providers.io.RepositoryPath;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;

import java.nio.file.Files;
import java.nio.file.Path;

/**
 * @author leipenghui
 * @date 2024/3/20
 **/
@AllArgsConstructor
@NoArgsConstructor
@Builder
@Data
@Slf4j
public class ConanArtifactIndex {

    public static final String INDEX_JSON_NAME = "index.json";

    private String user;
    private String name;
    private String version;
    private String channel;
    private String revisionId;
    private String packageId;
    private String indexRelativizePath;
    private String rootIndexRelativizePath;

    public static ConanArtifactIndex parse(String relativizePath) {
        if (StringUtils.isBlank(relativizePath)) {
            return null;
        }
        ConanArtifactIndex conanArtifactIndex = null;
        String[] pathArr = relativizePath.split(GlobalConstants.SEPARATOR);
        if (pathArr.length >= 7) {
            String packagePath = "";
            if (relativizePath.contains("/package/")) {
                packagePath = packagePath.concat(GlobalConstants.SEPARATOR).concat("package");
            }
            String indexRelativizePath = pathArr[0].concat(GlobalConstants.SEPARATOR).concat(pathArr[1]).concat(GlobalConstants.SEPARATOR).concat(pathArr[2]).concat(GlobalConstants.SEPARATOR).concat(pathArr[3]).concat(GlobalConstants.SEPARATOR).concat(pathArr[4]).concat(packagePath).concat(GlobalConstants.SEPARATOR).concat(pathArr[6]);
            String rootIndexRelativizePath = pathArr[0].concat(GlobalConstants.SEPARATOR).concat(pathArr[1]).concat(GlobalConstants.SEPARATOR).concat(pathArr[2]).concat(GlobalConstants.SEPARATOR).concat(pathArr[3]);
            conanArtifactIndex = ConanArtifactIndex.builder().user(pathArr[0]).name(pathArr[1]).version(pathArr[2]).channel(pathArr[3]).revisionId(pathArr[4]).packageId(pathArr[6]).indexRelativizePath(indexRelativizePath).rootIndexRelativizePath(rootIndexRelativizePath).build();
        } else if (pathArr.length >= 4) {
            String indexRelativizePath = pathArr[0].concat(GlobalConstants.SEPARATOR).concat(pathArr[1]).concat(GlobalConstants.SEPARATOR).concat(pathArr[2]).concat(GlobalConstants.SEPARATOR).concat(pathArr[3]);
            conanArtifactIndex = ConanArtifactIndex.builder().user(pathArr[0]).name(pathArr[1]).version(pathArr[2]).channel(pathArr[3]).indexRelativizePath(indexRelativizePath).build();
        }
        return conanArtifactIndex;
    }

    public static boolean isIndexDirectory(Path path) {
        if (!(path instanceof RepositoryPath)) {
            return false;
        }
        boolean flag = false;
        try {
            RepositoryPath repositoryPath = (RepositoryPath) path;
            boolean ignore = RepositoryFiles.isHidden(repositoryPath) || RepositoryFiles.isArtifactMetadata(repositoryPath) || RepositoryFiles.isTrash(repositoryPath) || RepositoryFiles.isTemp(repositoryPath);
            if (ignore) {
                return false;
            }
            String relativizePath = RepositoryFiles.relativizePath(repositoryPath);
            String[] pathArr = relativizePath.split(GlobalConstants.SEPARATOR);
            if (pathArr.length == 7 || pathArr.length == 4) {
                flag = true;
            }
        } catch (Exception ex) {
            log.error(ExceptionUtils.getStackTrace(ex));
        }
        return flag;
    }

    public static boolean include(Path path) {
        if (!(path instanceof RepositoryPath)) {
            return false;
        }
        try {
            RepositoryPath repositoryPath = (RepositoryPath) path;
            boolean ignore = !Files.isDirectory(path) || RepositoryFiles.isHidden(repositoryPath) || RepositoryFiles.isArtifactMetadata(repositoryPath) || RepositoryFiles.isTrash(repositoryPath) || RepositoryFiles.isTemp(repositoryPath);
            if (ignore) {
                return false;
            }
            return Files.isDirectory(path);
        } catch (Exception ex) {
            log.error(ExceptionUtils.getStackTrace(ex));
        }
        return false;
    }
}
