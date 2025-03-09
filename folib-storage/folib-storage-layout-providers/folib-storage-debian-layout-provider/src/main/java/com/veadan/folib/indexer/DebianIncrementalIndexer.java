package com.veadan.folib.indexer;

import com.google.common.collect.Lists;
import com.google.common.collect.Sets;
import com.veadan.folib.artifact.coordinates.DebianArtifactCoordinates;
import com.veadan.folib.cache.DebianPackagesMetadataCache;
import com.veadan.folib.constant.DebianConstant;
import com.veadan.folib.domain.Artifact;
import com.veadan.folib.domain.ArtifactWithMetadata;
import com.veadan.folib.domain.DebianPackagesContext;
import com.veadan.folib.domain.DebianReleaseContext;
import com.veadan.folib.enums.DeltaIndexEventType;
import com.veadan.folib.event.DebianIndexEvent;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.providers.io.RepositoryPathResolver;
import com.veadan.folib.services.ArtifactManagementService;
import com.veadan.folib.storage.repository.Repository;
import com.veadan.folib.util.ArtifactWithMetadataTransformer;
import com.veadan.folib.util.DebianUtils;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Component;

import javax.annotation.Resource;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/**
 * @author huayanjun
 * @since 2024-09-03 14:57
 * 增量更新只是更新特定/发行版/组件/架构的索引
 */
@Slf4j
@Component
public class DebianIncrementalIndexer {

    @Value("${folib.temp}")
    public String tempPath;

    @Resource
    private DebianPackagesMetadataCache debianPackagesMetadataCache;

    @Resource
    private RepositoryPathResolver resolver;

    @Resource
    private ArtifactManagementService artifactManagementService;

    @Resource
    private ArtifactorySearch artifactorySearch;

    @Async
    public void index(Repository repository, Set<DebianIndexEvent> indexEvents) {
        if (indexEvents == null || indexEvents.isEmpty()) {
            log.info("The incremental indexer does not handle entire-repo index operations, aborting");
        } else {
            log.debug("Creating additional index events as required for repo {}.", repository.getId());
            Set<DebianIndexEvent> events = this.createAdditionalIndexEvents(repository, indexEvents);
            log.debug("Finished creating additional index events as required for repo {},", repository.getId());
            events.stream().collect(Collectors.groupingBy(DebianIndexEvent::getDistribution)).entrySet().stream().filter(Objects::nonNull).filter(e -> e.getValue().size() > 0).forEach(distributionEvents -> {
                this.indexDistribution(repository, distributionEvents);
            });
        }
    }

    private void indexDistribution(Repository repo, Map.Entry<String, List<DebianIndexEvent>> eventsEntry) {
        List<String> indicesToDelete = Lists.newArrayList();
        Set<DebianPackagesContext> writtenIndices = Sets.newHashSet();
        String distribution = eventsEntry.getKey();
        log.info("Indexing all required packages of distribution {} on repo {}", distribution, repo.getId());
        eventsEntry.getValue().stream().collect(Collectors.groupingBy(DebianIndexEvent::componentArchitectureGroup)).entrySet().stream().filter(Objects::nonNull).filter(coordinateEvents -> coordinateEvents.getValue().size() > 0).forEach(coordinateEvents -> {
            this.indexSingleCoordinate(repo, distribution, coordinateEvents, writtenIndices, indicesToDelete);
        });
        this.indexRelease(repo, distribution, indicesToDelete);
//            DebianUtils.addByHashPackages(repo, writtenIndices, packagesArtifacts);
        finalizeIndex(repo, indicesToDelete);
        log.trace("Finished index for all required packages of distribution {}", distribution);
    }

    // 添加额外需要更新的索引 1.全架构索引2.强制架构索引
    private Set<DebianIndexEvent> createAdditionalIndexEvents(Repository repo, Set<DebianIndexEvent> allIndexEvents) {
        this.addCalcEventsForAllArch(repo, allIndexEvents);
        return allIndexEvents;
    }

    private void addCalcEventsForAllArch(Repository repo, Set<DebianIndexEvent> events) {
        Set<DebianIndexEvent> result = Sets.newHashSet();
        events.stream().filter(e -> "all".equals(e.getArchitecture())).collect(Collectors.groupingBy(DebianIndexEvent::distributionComponentGroup)).forEach((distAndComp, componentEvents) -> {
            String[] distComp = distAndComp.split(":");
            if (distComp.length != 2) {
                log.warn("Failed to resolve extra required incremental index events for component {}", distAndComp);
            }
            this.addEventsForComponent(repo, distComp[0], distComp[1], componentEvents, result);
        });
        events.addAll(result);
    }

    private void createIndexEventsForExistingArchitectures(List<Artifact> componentArtifacts, final DebianIndexEvent event, final Set<DebianIndexEvent> result) {
        componentArtifacts.stream().map(DebianUtils::getArchitectureFromPath)
                .map(arch -> new DebianIndexEvent(event, arch)).forEach(result::add);
    }

    private void indexSingleCoordinate(Repository repo, String distribution, Map.Entry<String, List<DebianIndexEvent>> eventsEntry, Set<DebianPackagesContext> writtenIndices, List<String> indicesToDelete) {
        DebianPackagesContext context = DebianUtils.createDebianPackagesContext(distribution, eventsEntry);
        if (context != null) {
            List<DebianIndexEvent> events = eventsEntry.getValue();
            try {
                Set<String> deleteEvents = this.getDeleteEvents(events);
                Set<String> addEvents = this.getAddEvents(repo, events, context);
                (new DebianPackagesMetadataIncrementalIndexer(this, repo, addEvents, deleteEvents, context, writtenIndices, indicesToDelete, resolver, artifactManagementService)).indexPackages();
            } catch (IOException e) {
                log.error("Failed indexing coordinate ", e);
            }
        }
    }

    private void indexRelease(Repository repo, String distribution, List<String> indicesToDelete) {
        log.debug("Indexing Release file for distribution {} on repo {}", distribution, repo.getId());
        new DebianReleaseMetadataIndexer(repo, indicesToDelete, resolver).indexRelease(distribution);
        log.trace("Index of Release file for distribution {} in repo {} ", distribution, repo.getId());

    }

    private Set<String> getAddEvents(Repository repo, List<DebianIndexEvent> events, DebianPackagesContext context) {
        String repoId = repo.getId();
        String coordinates = DebianUtils.print(context);
        log.debug("Extracting artifact metadata for coordinate {}", DebianUtils.print(context));
        ArtifactWithMetadataTransformer transformer = new ArtifactWithMetadataTransformer(repo, debianPackagesMetadataCache, this);
        Stream<Artifact> stream = events.stream().filter(event -> DeltaIndexEventType.ADD.equals(event.getEventType()) || DeltaIndexEventType.OVERRIDE.equals(event.getEventType())).map(DebianIndexEvent::getArtifact).filter(Objects::nonNull);
        Set<String> addEvents = stream.map(transformer).map(ArtifactWithMetadata::getMetadata).filter(Objects::nonNull).map((metadata) -> metadata.controlFileContent).collect(Collectors.toSet());
        log.debug("Reading metadata for coordinates {} (repo {}) from cache", coordinates, repoId);
        Objects.requireNonNull(addEvents);
        events.stream().filter(event -> DeltaIndexEventType.ADD.equals(event.getEventType()) || DeltaIndexEventType.OVERRIDE.equals(event.getEventType())).map(DebianIndexEvent::getArtifact).filter(Objects::isNull).map((noContent) -> "").forEach(addEvents::add);
        return addEvents;
    }

    private Set<String> getDeleteEvents(List<DebianIndexEvent> events) {
        return events.stream().filter(event -> DeltaIndexEventType.DELETE.equals(event.getEventType()) || DeltaIndexEventType.OVERRIDE.equals(event.getEventType())).map(DebianIndexEvent::getArtifact).filter(Objects::nonNull).map(Artifact::getArtifactPath).collect(Collectors.toSet());
    }


    private void addEventsForComponent(Repository repo, String distribution, String component, List<DebianIndexEvent> componentEvents, Set<DebianIndexEvent> result) {
        Set<String> forcedArchitectures = Sets.newHashSet();
        try {
            List<Artifact> componentArtifacts = artifactorySearch.findByDistributionAndComponent(distribution, component, repo);
            componentEvents.forEach((componentEvent) -> {
                this.createIndexEventsForExistingArchitectures(componentArtifacts, componentEvent, result);
            });
            this.createIndexEventsForForcedArchitectures(forcedArchitectures, componentEvents, result);
        } catch (Exception e) {
            log.warn("Failed to resolve extra required incremental index events for component {}: {}", distribution + "/" + component, e.getMessage());
        }
    }


    private void createIndexEventsForForcedArchitectures(Set<String> forcedArchitectures, final List<DebianIndexEvent> events, final Set<DebianIndexEvent> result) {
        events.stream().filter((event) -> !forcedArchitectures.contains(event.getArchitecture())).forEach((event) -> {
            forcedArchitectures.stream().map((forcedArch) -> new DebianIndexEvent(event, forcedArch)).forEach(result::add);
        });
    }

    public InputStream getPackageByRepo(Repository repo, String path) throws IOException {
        RepositoryPath packagePath = resolver.resolve(repo, path);
        Path target = packagePath.getTarget();
        if (!Files.exists(target)) {
            Files.createDirectories(target.getParent());
            Files.createFile(target);
        }
        return Files.newInputStream(packagePath.getTarget());
    }


    public void removeByPath(Repository repo, String path) {
        List<Artifact> packages = artifactorySearch.findAllPackage(repo);
        Map<String, List<Artifact>> collects = packages.stream().collect(Collectors.groupingBy(e -> e.getArtifactCoordinates().getCoordinates().get(DebianConstant.DISTRIBUTION)));
        for (Map.Entry<String, List<Artifact>> architectures : collects.entrySet()) {
            String distribution = architectures.getKey();
            List<Artifact> packageFile = architectures.getValue();
            List<String> indicesToDelete = Lists.newArrayList();
            for (Artifact artifact : packageFile) {
                log.info("reindex for {}", artifact.getArtifactPath());
                String artifactPath = artifact.getArtifactPath();
                File tempPackagesFile = null;
                RepositoryPath repositoryPath = resolver.resolve(repo, artifactPath);
                try (InputStream currentPackagesFile = Files.newInputStream(repositoryPath)) {
                    tempPackagesFile = this.getTempPackagesFilePath(tempPath);
                    DebianIndexIncrementalFilter indexFilter = new DebianIndexIncrementalFilter(Collections.emptySet(), Collections.singleton(path));
                    IncrementalIndexStreamer indexStream = new IncrementalIndexStreamer(currentPackagesFile, tempPackagesFile, indexFilter);
                    indexStream.write();
                    DebianArtifactCoordinates co = DebianArtifactCoordinates.parse(artifactPath);
                    DebianPackagesContext context = new DebianPackagesContext(co.getDistribution(), co.getComponent(), co.getArchitecture());
                    if (DebianPackagesMetadataIncrementalIndexer.isMarkedForDeletion(tempPackagesFile)) {
                        indicesToDelete.addAll(DebianUtils.pathsToPackagesFiles(context));
                        // 删除空的package
                        finalizeIndex(repo, indicesToDelete);
                    } else {
                        DebianPackagesMetadataIndexerBase indexerBase = new DebianPackagesMetadataIndexerBase(repo, this.resolver, this.artifactManagementService);
                        indexerBase.writePackagesFileContentToRepo(context, tempPackagesFile);
                    }
                } catch (Exception e) {
                    log.error("index {} failed", artifact.getArtifactPath(), e);
                } finally {
                    DebianUtils.deleteTempFile(tempPackagesFile);
                }
            }
            this.indexRelease(repo, distribution, indicesToDelete);
        }
    }

    protected File getTempPackagesFilePath(String tempPath) throws IOException {
        Path tempDir = Paths.get(tempPath);
        return Files.createTempFile(tempDir, "deb", "metadata").toFile();
    }

    //删除空的package
    public void finalizeIndex(Repository repo, List<String> indicesToDelete) {
        for (String packageFileToDelete : indicesToDelete) {
            RepositoryPath repositoryPath = resolver.resolve(repo, packageFileToDelete);
            try {
                Files.delete(repositoryPath);
            } catch (IOException e) {
                log.error("delete package:{} failed", repositoryPath.getTarget(), e);
            }
        }
    }
}
