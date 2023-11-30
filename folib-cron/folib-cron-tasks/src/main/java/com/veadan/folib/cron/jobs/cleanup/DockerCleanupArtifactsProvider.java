package com.veadan.folib.cron.jobs.cleanup;

import com.google.common.collect.Lists;
import com.google.common.collect.Maps;
import com.veadan.folib.domain.Artifact;
import com.veadan.folib.providers.io.RepositoryFiles;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.providers.io.RepositoryPathResolver;
import com.veadan.folib.services.ArtifactManagementService;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.springframework.stereotype.Component;

import javax.annotation.PostConstruct;
import javax.inject.Inject;
import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.attribute.FileTime;
import java.time.LocalDateTime;
import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/**
 * @author leipenghui
 **/
@Slf4j
@Component
public class DockerCleanupArtifactsProvider implements CleanupArtifactsProvider {

    @Inject
    private CleanupArtifactsProviderRegistry cleanupArtifactsProviderRegistry;

    @Inject
    private ArtifactManagementService artifactManagementService;

    @Inject
    private RepositoryPathResolver repositoryPathResolver;

    private static final Integer ONE = 1;

    private static final Integer ZERO = 0;

    private static final Long MINUS_ONE = -1L;

    private static final String PREFIX = "sha256:";

    @PostConstruct
    @Override
    public void register() {
        cleanupArtifactsProviderRegistry.addProvider("DOCKER", this);
        log.info("Registered cleanup repository cron job '[{}]' with alias '[{}]'.",
                getClass().getCanonicalName(), "DOCKER");
    }

    @Override
    public void cleanup(String storageId, String repositoryId, String path, String storageDay, String storageCondition) throws Exception {
        try {
            RepositoryPath repositoryPath = repositoryPathResolver.resolve(storageId, repositoryId);
            String blobs = "blobs", manifest = "manifest", tag = "tag";
            List<String> excludeList = Lists.newArrayList(blobs, manifest);
            List<Path> repositoryPathList = getDirectory(repositoryPath, Collections.emptyList());
            if (CollectionUtils.isEmpty(repositoryPathList)) {
                log.info("Repository storageId [{}] repositoryId [{}] not found artifacts", storageId, repositoryId);
                return;
            }
            log.info("Start cleanup artifact job storageId [{}] repositoryId [{}] storageCondition [{}] storageDay [{}] image quantity [{}]", storageId, repositoryId, storageCondition, storageDay, repositoryPathList.size());
            List<Integer> resultList = Lists.newArrayList();
            RepositoryPath imageRepositoryPath;
            for (Path imagePath : repositoryPathList) {
                try {
                    imageRepositoryPath = (RepositoryPath) imagePath;
                    log.info("Cleanup artifact job storageId [{}] repositoryId [{}] storageCondition [{}] storageDay [{}] imagePath [{}]", storageId, repositoryId, storageCondition, storageDay, imageRepositoryPath);
                    if (tag.equalsIgnoreCase(storageCondition)) {
                        //按照tag保留
                        handlerTag(storageId, repositoryId, storageDay, storageCondition, imageRepositoryPath, excludeList, resultList);
                    } else {
                        //按照天数保留
                        handlerDay(storageId, repositoryId, storageDay, storageCondition, imageRepositoryPath, excludeList, resultList);
                    }
                } catch (Exception ex) {
                    log.error("Cleanup artifact job storageId [{}] repositoryId [{}] storageCondition [{}] storageDay [{}] path [{}] error [{}]", storageId, repositoryId, storageCondition, storageDay, path, ExceptionUtils.getStackTrace(ex));
                }
            }
            long success = resultList.stream().filter(ONE::equals).count(), fail = resultList.stream().filter(ZERO::equals).count();
            log.info("Cleanup artifact job storageId [{}] repositoryId [{}] storageCondition [{}] storageDay [{}] success [{}] fail [{}]",
                    storageId, repositoryId, storageCondition, storageDay, success, fail);
        } catch (Exception e) {
            log.error("Cleanup artifact job storageId [{}] repositoryId [{}] storageCondition [{}] storageDay [{}] error [{}]", storageId, repositoryId, storageCondition, storageDay, ExceptionUtils.getStackTrace(e));
        }
    }

    private void handlerTag(String storageId, String repositoryId, String storageDay, String storageCondition, RepositoryPath imageRepositoryPath, List<String> excludeList, List<Integer> resultList) throws Exception {
        Long storageQuantity = Long.parseLong(storageDay);
        List<Path> tagRepositoryPathList = getDirectory(imageRepositoryPath, excludeList);
        log.info("Cleanup artifact job storageId [{}] repositoryId [{}] storageCondition [{}] storage quantity [{}] imagePath [{}] tag quantity [{}] tags [{}]", storageId, repositoryId, storageCondition, storageQuantity, imageRepositoryPath, tagRepositoryPathList.size(), tagRepositoryPathList.stream().map(p -> p.getFileName().toString()).collect(Collectors.joining(",")));
        if (CollectionUtils.isEmpty(tagRepositoryPathList) || tagRepositoryPathList.size() <= storageQuantity) {
            log.info("Cleanup artifact job storageId [{}] repositoryId [{}] storageCondition [{}] imagePath [{}] tag quantity [{}] less than or equal storage quantity [{}] skip", storageId, repositoryId, storageCondition, imageRepositoryPath, tagRepositoryPathList.size(), storageQuantity);
            return;
        }
        RepositoryPath repositoryPath, manifestRepositoryPath;
        Map<RepositoryPath, Long> map = Maps.newHashMap();
        Artifact artifact = null;
        for (Path path : tagRepositoryPathList) {
            log.info("Cleanup artifact job storageId [{}] repositoryId [{}] storageCondition [{}] storage quantity [{}] imagePath [{}] tag [{}]", storageId, repositoryId, storageCondition, storageQuantity, imageRepositoryPath, path);
            repositoryPath = (RepositoryPath) path;
            manifestRepositoryPath = getManifestPath(repositoryPath);
            if (Objects.isNull(manifestRepositoryPath)) {
                log.warn("Cleanup storageId [{}] repositoryId [{}] path [{}] manifest not found", storageId, repositoryId, repositoryPath);
                continue;
            }
            artifact = manifestRepositoryPath.getArtifactEntry();
            if (Objects.isNull(artifact)) {
                log.warn("Cleanup storageId [{}] repositoryId [{}] path [{}] artifact not found", storageId, repositoryId, manifestRepositoryPath);
                continue;
            }
            Map<String, Object> fileAttributes = Files.readAttributes(manifestRepositoryPath, "*");
            map.put(manifestRepositoryPath, ((FileTime) fileAttributes.get("creationTime")).toMillis());
        }
        map = map.entrySet().stream()
                .sorted(Map.Entry.comparingByValue())
                .collect(Collectors.toMap(Map.Entry::getKey, Map.Entry::getValue,
                        (oldValue, newValue) -> oldValue, LinkedHashMap::new));
        log.info("Cleanup artifact job storageId [{}] repositoryId [{}] storageCondition [{}] storage quantity [{}] imagePath [{}] tag quantity [{}] order tags [{}]", storageId, repositoryId, storageCondition, storageQuantity, imageRepositoryPath, tagRepositoryPathList.size(), map.keySet().toString());
        for (Map.Entry<RepositoryPath, Long> manifestEntry : map.entrySet()) {
            Long currentTagSize = getTagSize(imageRepositoryPath, excludeList);
            log.info("Cleanup artifact job storageId [{}] repositoryId [{}] storageCondition [{}] storage quantity [{}] imagePath [{}] tag [{}] currentTagSize [{}]", storageId, repositoryId, storageCondition, storageQuantity, imageRepositoryPath, manifestEntry.getKey(), currentTagSize);
            if (currentTagSize > storageQuantity) {
                Integer result = cleanupArtifact(manifestEntry.getKey(), storageId, repositoryId, MINUS_ONE.toString());
                if (Objects.nonNull(result)) {
                    resultList.add(result);
                }
            } else {
                log.info("Cleanup artifact job storageId [{}] repositoryId [{}] storageCondition [{}] storage quantity [{}] imagePath [{}] tag [{}] currentTagSize [{}] cleanup over", storageId, repositoryId, storageCondition, storageQuantity, imageRepositoryPath, manifestEntry.getKey(), currentTagSize);
                break;
            }
        }
    }

    private void handlerDay(String storageId, String repositoryId, String storageDay, String storageCondition, RepositoryPath imageRepositoryPath, List<String> excludeList, List<Integer> resultList) throws Exception {
        List<Path> tagRepositoryPathList = getDirectory(imageRepositoryPath, excludeList);
        log.info("Cleanup artifact job storageId [{}] repositoryId [{}] storageCondition [{}] storage quantity [{}] imagePath [{}] tags [{}]", storageId, repositoryId, storageCondition, storageDay, imageRepositoryPath, tagRepositoryPathList.stream().map(p -> p.getFileName().toString()).collect(Collectors.joining(",")));
        if (CollectionUtils.isEmpty(tagRepositoryPathList)) {
            return;
        }
        RepositoryPath tagRepositoryPath;
        for (Path tagPath : tagRepositoryPathList) {
            tagRepositoryPath = (RepositoryPath) tagPath;
            log.info("Cleanup artifact job storageId [{}] repositoryId [{}] storageCondition [{}] storageDay [{}] imagePath [{}] tagPath [{}]", storageId, repositoryId, storageCondition, storageDay, imageRepositoryPath, tagPath);
            try {
                Integer result = cleanupArtifact(getManifestPath(tagRepositoryPath), storageId, repositoryId, storageDay);
                if (Objects.nonNull(result)) {
                    resultList.add(result);
                }
            } catch (Exception ex) {
                log.error("Cleanup artifact job storageId [{}] repositoryId [{}] storageCondition [{}] storageDay [{}] imagePath [{}] tagPath [{}] error [{}]", storageId, repositoryId, storageCondition, storageDay, imageRepositoryPath, tagPath, ExceptionUtils.getStackTrace(ex));
            }
        }
    }

    private Integer cleanupArtifact(RepositoryPath repositoryPath, String storageId, String repositoryId, String storageDay) throws Exception {
        long tempDay = Long.parseLong(storageDay);
        if (Objects.isNull(repositoryPath) || !Files.exists(repositoryPath)) {
            log.warn("Cleanup storageId [{}] repositoryId [{}] path [{}] file not exists", storageId, repositoryId, repositoryPath);
            return null;
        }
        if (Files.isDirectory(repositoryPath)) {
            log.warn("Cleanup storageId [{}] repositoryId [{}] path [{}] is directory skip", storageId, repositoryId, repositoryPath);
            return null;
        }
        String path = repositoryPath.toString();
        boolean checkDockerTag = repositoryPath.getFileName().toString().startsWith(PREFIX) && !path.contains("blobs/sha256") && !path.contains("manifest/sha256");
        if (!checkDockerTag) {
            log.info("Cleanup storageId [{}] repositoryId [{}] path [{}] not a docker tag file skip", storageId, repositoryId, path);
            return null;
        }
        if (RepositoryFiles.isTrash(repositoryPath)) {
            log.info("Cleanup storageId [{}] repositoryId [{}] path [{}] is trash file skip", storageId, repositoryId, path);
            return null;
        }
        if (RepositoryFiles.isTemp(repositoryPath)) {
            log.info("Cleanup storageId [{}] repositoryId [{}] path [{}] is temp file skip", storageId, repositoryId, path);
            return null;
        }
        if (RepositoryFiles.isChecksum(repositoryPath)) {
            log.info("Cleanup storageId [{}] repositoryId [{}] path [{}] is checksum file skip", storageId, repositoryId, path);
            return null;
        }
        if (!RepositoryFiles.isArtifact(repositoryPath)) {
            log.info("Cleanup storageId [{}] repositoryId [{}] path [{}] not is artifact file skip", storageId, repositoryId, path);
            return null;
        }
        log.info("Cleanup storageId [{}] repositoryId [{}] path [{}] is a docker tag file", storageId, repositoryId, path);
        Artifact artifact = repositoryPath.getArtifactEntry();
        if (null == artifact || null == artifact.getLastUsed()) {
            log.warn("Cleanup storageId [{}] repositoryId [{}] path [{}] artifact not found", storageId, repositoryId, path);
            return null;
        }
        RepositoryPath manifestRepositoryPath = repositoryPathResolver.resolve(repositoryPath.getStorageId(), repositoryPath.getRepositoryId(), repositoryPath.getParent().getParent().getFileName().toString() + File.separator + "manifest" + File.separator + repositoryPath.getFileName().toString());
        if (!Files.exists(manifestRepositoryPath)) {
            log.warn("Cleanup storageId [{}] repositoryId [{}] path [{}] manifest file not exists", storageId, repositoryId, manifestRepositoryPath.toString());
            return null;
        }
        Artifact manifestArtifact = manifestRepositoryPath.getArtifactEntry();
        if (null == manifestArtifact || null == manifestArtifact.getLastUsed()) {
            log.warn("Cleanup storageId [{}] repositoryId [{}] path [{}] manifest artifact not found", storageId, repositoryId, manifestRepositoryPath.toString());
            return null;
        }
        //获取仓库下制品最近使用时间做比较
        LocalDateTime tagTime = artifact.getLastUsed();
        LocalDateTime manifestTime = manifestArtifact.getLastUsed();
        log.info("Cleanup docker storageId [{}] repositoryId [{}] storageDay [{}] path [{}] time [{}] manifest time [{}] current time [{}]", storageId, repositoryId, storageDay, artifact.getArtifactPath(), tagTime, manifestTime, LocalDateTime.now());
        boolean canDelete = (!LocalDateTime.now().minusDays(tempDay).isBefore(tagTime) && !LocalDateTime.now().minusDays(tempDay).isBefore(manifestTime)) || MINUS_ONE.equals(tempDay);
        if (canDelete) {
            try {
                RepositoryPath deleteRepositoryPath = repositoryPath.getParent();
                log.info("Cleanup docker tag storageId [{}] repositoryId [{}] path [{}] do delete", storageId, repositoryId, deleteRepositoryPath.toString());
                artifactManagementService.delete(deleteRepositoryPath, true);
                RepositoryPath dockerImageRepositoryPath = deleteRepositoryPath.getParent();
                if (Files.exists(dockerImageRepositoryPath) && !Files.isSameFile(repositoryPath.getRoot(), dockerImageRepositoryPath) && Files.list(dockerImageRepositoryPath).count() == 0) {
                    Files.deleteIfExists(dockerImageRepositoryPath);
                    log.info("Cleanup docker image storageId [{}] repositoryId [{}] path [{}] do delete", storageId, repositoryId, dockerImageRepositoryPath.toString());
                }
                return ONE;
            } catch (Exception e) {
                log.error("Cleanup storageId [{}] repositoryId [{}] path [{}] error [{}]", storageId, repositoryId, path, ExceptionUtils.getStackTrace(e));
                return ZERO;
            }
        }
        return null;
    }

    /**
     * 获取仓库下的目录列表
     *
     * @param repositoryPath 仓库路径
     * @param excludeList    要排除的列表
     * @return 目录列表
     * @throws IOException 异常
     */
    private List<Path> getDirectory(RepositoryPath repositoryPath, List<String> excludeList)
            throws IOException {
        List<Path> directoryList;
        try (Stream<Path> pathStream = Files.list(repositoryPath)) {
            directoryList = pathStream.filter(p -> !p.toString().startsWith(".") && !p.toString().contains("/.") && excludeList.stream().noneMatch(p.getFileName().toString()::equals))
                    .filter(p -> {
                        try {
                            return !Files.isHidden(p) && Files.isDirectory(p);
                        } catch (IOException e) {
                            log.warn("Error accessing path [{}] error [{}]", p, ExceptionUtils.getStackTrace(e));
                            return false;
                        }
                    })
                    .sorted()
                    .collect(Collectors.toList());
        }
        return directoryList;
    }

    /**
     * 获取tag个数
     *
     * @param repositoryPath 制品路径
     * @param excludeList    要排除的列表
     * @return tag个数
     * @throws IOException 异常
     */
    private Long getTagSize(RepositoryPath repositoryPath, List<String> excludeList)
            throws IOException {
        Long size;
        try (Stream<Path> pathStream = Files.list(repositoryPath)) {
            size = pathStream.filter(p -> !p.toString().startsWith(".") && !p.toString().contains("/.") && excludeList.stream().noneMatch(p.getFileName().toString()::equals))
                    .filter(p -> {
                        try {
                            return !Files.isHidden(p) && Files.isDirectory(p);
                        } catch (IOException e) {
                            log.warn("Error accessing path [{}] error [{}]", p, ExceptionUtils.getStackTrace(e));
                            return false;
                        }
                    })
                    .count();
        }
        return size;
    }

    /**
     * 获取仓库下的tag路径
     *
     * @param repositoryPath 仓库路径
     * @return tag路径
     * @throws IOException 异常
     */
    private RepositoryPath getManifestPath(RepositoryPath repositoryPath)
            throws IOException {
        RepositoryPath path = null;
        List<String> excludeList = Lists.newArrayList("temp", ".temp");
        try (Stream<Path> pathStream = Files.list(repositoryPath)) {
            List<Path> pathList = pathStream.filter(p -> {
                try {
                    RepositoryPath itemRepositoryPath = (RepositoryPath) p;
                    return p.getFileName().toString().startsWith(PREFIX) && !p.toString().startsWith(".") && !p.toString().contains("/.") && excludeList.stream().noneMatch(p.getFileName().toString()::equals) &&
                            !Files.isHidden(p) && !Files.isDirectory(p) && !RepositoryFiles.isChecksum(itemRepositoryPath) && !RepositoryFiles.isArtifactMetadata(itemRepositoryPath);
                } catch (IOException e) {
                    log.warn("Error accessing path [{}] error [{}]", p, ExceptionUtils.getStackTrace(e));
                    return false;
                }
            })
                    .sorted()
                    .collect(Collectors.toList());
            if (CollectionUtils.isNotEmpty(pathList)) {
                path = (RepositoryPath) pathList.get(0);
                path.setArtifact(repositoryPathResolver.resolve(path.getStorageId(), path.getRepositoryId(), RepositoryFiles.relativizePath(path)).getArtifactEntry());

            }
            log.info("Tag [{}] manifestRepositoryPath [{}]", repositoryPath, path);
        }
        return path;
    }
}
