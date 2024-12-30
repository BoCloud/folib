package com.veadan.folib.locator.handlers;

import cn.hutool.extra.spring.SpringUtil;
import com.github.zafarkhaja.semver.Version;
import com.google.common.collect.Maps;
import com.veadan.folib.artifact.locator.handlers.AbstractArtifactLocationHandler;
import com.veadan.folib.providers.io.RepositoryFiles;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.providers.io.RootRepositoryPath;
import com.veadan.folib.storage.manager.RawArtifactManager;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.collections4.MapUtils;
import org.apache.commons.compress.utils.Lists;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.apache.maven.artifact.versioning.ComparableVersion;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Comparator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Stream;

/**
 * @author Kate Novik.
 */
public class RemoveRawArtifactOperation
        extends AbstractArtifactLocationHandler {

    private static final Logger logger = LoggerFactory.getLogger(
            RemoveRawArtifactOperation.class);

    private RepositoryPath previousPath;

    private int numberToKeep;

    private int keepPeriod;

    private Map<String, String> cleanupArtifactPathMap;


    public RemoveRawArtifactOperation() {
    }

    @Override
    public void execute(RepositoryPath basePath) throws IOException {
        if (!Files.exists(basePath)) {
            return;
        }
        LinkedHashMap<RepositoryPath, List<RepositoryPath>> visitedRootPaths = visitedRootPaths(basePath);
        if (MapUtils.isEmpty(visitedRootPaths)) {
            return;
        }
        RawArtifactManager rawArtifactManager = SpringUtil.getBean(RawArtifactManager.class);
        rawArtifactManager.deleteArtifacts(basePath, visitedRootPaths, numberToKeep, cleanupArtifactPathMap);
    }

    public int getNumberToKeep() {
        return numberToKeep;
    }

    public void setNumberToKeep(int numberToKeep) {
        this.numberToKeep = numberToKeep;
    }

    public int getKeepPeriod() {
        return keepPeriod;
    }

    public void setKeepPeriod(int keepPeriod) {
        this.keepPeriod = keepPeriod;
    }

    public Map<String, String> getCleanupArtifactPathMap() {
        return cleanupArtifactPathMap;
    }

    public void setCleanupArtifactPathMap(Map<String, String> cleanupArtifactPathMap) {
        this.cleanupArtifactPathMap = cleanupArtifactPathMap;
    }

    public LinkedHashMap<RepositoryPath, List<RepositoryPath>> visitedRootPaths(RepositoryPath basePath) throws IOException {
        LinkedHashMap<RepositoryPath, List<RepositoryPath>> visitedRootPaths = Maps.newLinkedHashMap();
        String customLayout = basePath.getRepository().getCustomLayout();
        if (StringUtils.isBlank(customLayout)) {
            return visitedRootPaths;
        }
        final Pattern pattern = Pattern.compile(customLayout);
        try (Stream<Path> pathStream = Files.walk(basePath)) {
            pathStream.forEach(p -> {
                try {
                    Map<RepositoryPath, RepositoryPath> matcherPathMap = matcherPath(pattern, p);
                    if (MapUtils.isNotEmpty(matcherPathMap)) {
                        matcherPathMap.forEach((key, value) -> {
                            List<RepositoryPath> repositoryPathList = visitedRootPaths.get(key);
                            if (CollectionUtils.isEmpty(repositoryPathList)) {
                                repositoryPathList = Lists.newArrayList();
                                visitedRootPaths.put(key, repositoryPathList);
                            }
                            if (!repositoryPathList.contains(value)) {
                                repositoryPathList.add(value);
                                repositoryPathList.sort(Comparator.comparing(o -> new ComparableVersion(o.getFileName().toString())));
                            }
                        });
                    }
                } catch (Exception ex) {
                    logger.error("VersionDirectories error [{}]", ExceptionUtils.getStackTrace(ex));
                }
            });
            return visitedRootPaths;
        }
    }

    private Map<RepositoryPath, RepositoryPath> matcherPath(Pattern pattern, Path path) {
        try {
            RepositoryPath repositoryPath = (RepositoryPath) path;
            if (RepositoryFiles.isTrash(repositoryPath) || RepositoryFiles.isHidden(repositoryPath) || !RepositoryFiles.isArtifact(repositoryPath)) {
                return null;
            }
            String artifactPath = RepositoryFiles.relativizePath(repositoryPath);
            Matcher matcher = pattern.matcher(artifactPath);
            if (matcher.matches()) {
                Map<RepositoryPath, RepositoryPath> repositoryPathMap = Maps.newHashMap();
                String orgPath = matcher.group("orgPath");
                String module = matcher.group("module");
                String baseRev = matcher.group("baseRev");
                String prefixPath = String.format("%s/%s/%s", orgPath, module, baseRev);
                RootRepositoryPath rootRepositoryPath = repositoryPath.getFileSystem().getRootDirectory();
                RepositoryPath repositoryPathKey = rootRepositoryPath.resolve(orgPath).resolve(module);
                if (artifactPath.startsWith(prefixPath)) {
                    repositoryPathMap.put(repositoryPathKey, repositoryPath.getParent());
                    return repositoryPathMap;
                }
                repositoryPathMap.put(repositoryPathKey, repositoryPath);
                return repositoryPathMap;
            }
            return null;
        } catch (Exception ex) {
            logger.error("Path [{}] isMatcher error [{}]", path.toString(), ExceptionUtils.getStackTrace(ex));
        }
        return null;
    }

}
