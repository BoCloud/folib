package com.veadan.folib.repositories;

import cn.hutool.core.date.DatePattern;
import cn.hutool.core.date.DateUtil;
import com.google.common.collect.Maps;
import com.google.common.collect.Sets;
import com.veadan.folib.artifact.coordinates.ArtifactLayoutDescription;
import com.veadan.folib.artifact.coordinates.ArtifactLayoutLocator;
import com.veadan.folib.configuration.ConfigurationManager;
import com.veadan.folib.configuration.ConfigurationUtils;
import com.veadan.folib.db.schema.Edges;
import com.veadan.folib.db.schema.Properties;
import com.veadan.folib.db.schema.Vertices;
import com.veadan.folib.domain.Artifact;
import com.veadan.folib.domain.Vulnerability;
import com.veadan.folib.domain.VulnerabilityArtifactDomain;
import com.veadan.folib.enums.SafeLevelEnum;
import com.veadan.folib.enums.VulnerabilityPlatformEnum;
import com.veadan.folib.gremlin.adapters.ArtifactAdapter;
import com.veadan.folib.gremlin.dsl.EntityTraversal;
import com.veadan.folib.gremlin.dsl.EntityTraversalUtils;
import com.veadan.folib.gremlin.repositories.GremlinVertexRepository;
import com.veadan.folib.providers.io.RepositoryPathResolver;
import com.veadan.folib.services.ConfigurationManagementService;
import com.veadan.folib.services.support.ArtifactRoutingRulesChecker;
import com.veadan.folib.storage.repository.RepositoryTypeEnum;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.compress.utils.Lists;
import org.apache.commons.lang3.StringUtils;
import org.apache.tinkerpop.gremlin.process.traversal.Order;
import org.apache.tinkerpop.gremlin.process.traversal.P;
import org.apache.tinkerpop.gremlin.structure.Vertex;
import org.janusgraph.core.attribute.Text;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.Pageable;
import org.springframework.data.neo4j.annotation.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import javax.inject.Inject;
import javax.transaction.Transactional;
import java.time.LocalDateTime;
import java.util.*;

@Slf4j
@Repository
@Transactional
public class ArtifactRepository extends GremlinVertexRepository<Artifact> {
    //查找标记
    @Inject
    ArtifactAdapter artifactAdapter;
    @Inject
    ArtifactEntityQueries queries;
    @Inject
    ConfigurationManager configurationManager;
    @Inject
    ConfigurationManagementService configurationManagementService;
    @Inject
    RepositoryPathResolver repositoryPathResolver;
    @Inject
    ArtifactRoutingRulesChecker artifactRoutingRulesChecker;

    @Override
    protected ArtifactAdapter adapter() {
        return artifactAdapter;
    }

    public List<Artifact> findByPathLike(String storageId,
                                         String repositoryId,
                                         String path) {
        return EntityTraversalUtils.reduceHierarchy(queries.findByPathLike(storageId, repositoryId, path));
    }

    public Page<Artifact> findMatching(Integer lastAccessedTimeInDays,
                                       Long minSizeInBytes,
                                       Pageable pagination) {
        LocalDateTime date = Optional.ofNullable(lastAccessedTimeInDays)
                .map(v -> LocalDateTime.now().minusDays(lastAccessedTimeInDays))
                .orElse(null);
        return findMatching(date, minSizeInBytes, pagination);
    }

    public Page<Artifact> findMatching(LocalDateTime lastAccessedDate,
                                       Long minSizeInBytes,
                                       Pageable pagination) {
        Page<Artifact> result = queries.findMatching(lastAccessedDate, minSizeInBytes, pagination);

        return new PageImpl<>(EntityTraversalUtils.reduceHierarchy(result.toList()), pagination, result.getTotalElements());
    }

    public Page<Artifact> findMatching1(String artifactName,
                                        Pageable pagination) {
        Page<Artifact> result = queries.findMatching1(artifactName, pagination);

        return new PageImpl<>(EntityTraversalUtils.reduceHierarchy(result.toList()), pagination, result.getTotalElements());
    }

    public Page<Artifact> findMatching2(String artifactName,
                                        String storageId,
                                        String repositoryId,
                                        Pageable pagination) {
        Page<Artifact> result = queries.findMatching2(artifactName, storageId, repositoryId, pagination);
        return new PageImpl<>(EntityTraversalUtils.reduceHierarchy(result.toList()), pagination, result.getTotalElements());
    }

    public Boolean artifactEntityExists(String storageId,
                                        String repositoryId,
                                        String path) {
        return Optional.ofNullable(queries.artifactEntityExists(storageId, repositoryId, path)).orElse(Boolean.FALSE);
    }

    public Page<Artifact> findMatchingByIndex(Pageable pagination, Boolean regex, String artifactName,
                                              String metadataSearch,
                                              String storageId,
                                              String repositoryId,
                                              List<String> repositoryIds,
                                              List<String> storageIdAndRepositoryIdList,
                                              String beginDate,
                                              String endDate,
                                              String safeLevel,
                                              String sortField,
                                              String sortOrder) {
        com.veadan.folib.storage.repository.Repository repository = null;
        boolean isGroupRepository = false;
        if (StringUtils.isNotBlank(storageId) && StringUtils.isNotBlank(repositoryId) && CollectionUtils.isEmpty(storageIdAndRepositoryIdList)) {
            repository = configurationManager.getRepository(storageId, repositoryId);
            if (repository == null) {
                return new PageImpl<>(Collections.emptyList(), pagination, 0);
            }
            isGroupRepository = RepositoryTypeEnum.GROUP.getType().equals(repository.getType());
            if (isGroupRepository) {
                storageIdAndRepositoryIdList = getGroupStorageIdAndRepositoryId(repository);
                storageId = "";
                repositoryId = "";
            }
        }
        Long zero = 0L;
        Long count = buildEntityTraversal(regex, artifactName, metadataSearch, storageIdAndRepositoryIdList, storageId, repositoryId, repositoryIds, beginDate, endDate, safeLevel, sortField, sortOrder).count().tryNext().orElse(zero);
        if (zero.equals(count)) {
            return new PageImpl<>(Collections.emptyList(), pagination, count);
        }
        long low = pagination.getPageNumber() * pagination.getPageSize();
        long high = (pagination.getPageNumber() + 1) * pagination.getPageSize();


        List<Artifact> artifactList = buildEntityTraversal(regex, artifactName, metadataSearch, storageIdAndRepositoryIdList, storageId, repositoryId, repositoryIds, beginDate, endDate, safeLevel, sortField, sortOrder)
                .range(low, high)
                .map(artifactAdapter.fold(Optional.ofNullable(repository)
                        .map(com.veadan.folib.storage.repository.Repository::getLayout)
                        .map(ArtifactLayoutLocator.getLayoutByNameEntityMap()::get)
                        .map(ArtifactLayoutDescription::getArtifactCoordinatesClass))).toList();
        return new PageImpl<>(artifactList, pagination, count);
    }

    private List<String> getGroupStorageIdAndRepositoryId(com.veadan.folib.storage.repository.Repository repository) {
        List<String> storageIdAndRepositoryIdList = Lists.newArrayList();
        for (String storageAndRepositoryId : repository.getGroupRepositories()) {
            String sId = ConfigurationUtils.getStorageId(repository.getStorage().getId(), storageAndRepositoryId);
            String rId = ConfigurationUtils.getRepositoryId(storageAndRepositoryId);
            com.veadan.folib.storage.repository.Repository subRepository = configurationManagementService.getConfiguration().getRepository(sId, rId);
            if (!subRepository.isInService()) {
                continue;
            }
            if (!subRepository.isAllowsDirectoryBrowsing()) {
                continue;
            }
            storageIdAndRepositoryIdList.add(subRepository.getStorage().getId() + "-" + subRepository.getId());
        }
        return storageIdAndRepositoryIdList;
    }

    public Page<Artifact> scannerListByParams(Pageable pagination, String artifactName,
                                              String storageId,
                                              String repositoryId) {
        com.veadan.folib.storage.repository.Repository repository = null;
        if (StringUtils.isNotBlank(storageId) && StringUtils.isNotBlank(repositoryId)) {
            repository = configurationManager.getRepository(storageId, repositoryId);
        }
        Long zero = 0L;
        Long count = scannerListEntityTraversal(artifactName, storageId, repositoryId).count().tryNext().orElse(zero);
        if (zero.equals(count)) {
            return new PageImpl<>(Collections.emptyList(), pagination, count);
        }
        long low = pagination.getPageNumber() * pagination.getPageSize();
        long high = (pagination.getPageNumber() + 1) * pagination.getPageSize();
        List<Artifact> artifactList = scannerListEntityTraversal(artifactName, storageId, repositoryId)
                .range(low, high)
                .map(artifactAdapter.fold(Optional.ofNullable(repository)
                        .map(com.veadan.folib.storage.repository.Repository::getLayout)
                        .map(ArtifactLayoutLocator.getLayoutByNameEntityMap()::get)
                        .map(ArtifactLayoutDescription::getArtifactCoordinatesClass))).toList();
        return new PageImpl<>(artifactList, pagination, count);
    }

    private EntityTraversal<Vertex, Vertex> scannerListEntityTraversal(String artifactName,
                                                                       String storageId,
                                                                       String repositoryId) {
        EntityTraversal<Vertex, Vertex> entityTraversal = g().V().hasLabel(Vertices.ARTIFACT).has(Properties.STORAGE_ID, storageId).has(Properties.REPOSITORY_ID, repositoryId).has(Properties.SAFE_LEVEL, SafeLevelEnum.SCAN_COMPLETE.getLevel());
        if (StringUtils.isNotBlank(artifactName)) {
            entityTraversal = entityTraversal.has(Properties.UUID, Text.textContains(artifactName));
        }
        return entityTraversal;
    }

    public List<Artifact> findMatchingByVulnerabilityUuid(String vulnerabilityUuid,
                                                          String storageId,
                                                          List<String> storageIdAndRepositoryIdList) {
        List<Artifact> artifactList = buildEntityTraversalByVulnerabilityUuid(vulnerabilityUuid, storageId, storageIdAndRepositoryIdList)
                .map(artifactAdapter.fold()).toList();
        return EntityTraversalUtils.reduceHierarchy(artifactList);
    }

    public long countByVulnerabilityUuid(String vulnerabilityUuid,
                                         String storageId,
                                         List<String> storageIdAndRepositoryIdList) {
        return buildEntityTraversalByVulnerabilityUuid(vulnerabilityUuid, storageId, storageIdAndRepositoryIdList).count().tryNext().orElse(0L);
    }

    public List<Artifact> findMatchingBySafeLevels(List<String> storageIdAndRepositoryIdList, List<String> safeLevels) {
        List<Artifact> artifactList = g().V().hasLabel(Vertices.ARTIFACT).has(Properties.STORAGE_ID_AND_REPOSITORY_ID, P.within(storageIdAndRepositoryIdList)).has(Properties.SAFE_LEVEL, P.within(safeLevels)).map(artifactAdapter.fold()).toList();
        return EntityTraversalUtils.reduceHierarchy(artifactList);
    }

    public Long countByStorageIdAndRepositoryId(List<String> storageIdAndRepositoryIdList, String layout) {
        if ("Docker".equals(layout)) {
            EntityTraversal<Vertex, Vertex> entityTraversal = g().V().hasLabel(Vertices.ARTIFACT).has(Properties.UUID, Text.textNotContains("blobs/sha256")).has(Properties.UUID, Text.textNotContains("manifest/sha256")).has(Properties.ARTIFACT_FILE_EXISTS, true);
            if (CollectionUtils.isNotEmpty(storageIdAndRepositoryIdList)) {
                entityTraversal = entityTraversal.has(Properties.STORAGE_ID_AND_REPOSITORY_ID, P.within(storageIdAndRepositoryIdList));
            }
            return entityTraversal.count().tryNext().orElse(0L);
        }
        EntityTraversal<Vertex, Vertex> entityTraversal = g().V().hasLabel(Vertices.ARTIFACT).has(Properties.ARTIFACT_FILE_EXISTS, true);
        if (CollectionUtils.isNotEmpty(storageIdAndRepositoryIdList)) {
            entityTraversal = entityTraversal.has(Properties.STORAGE_ID_AND_REPOSITORY_ID, P.within(storageIdAndRepositoryIdList));
        }
        return entityTraversal.count().tryNext().orElse(0L);
    }

    public Map<String, Long> countArtifactByStorageIdAndRepositoryId(List<String> storageIdAndRepositoryIdList) {
        Long downloadCount = sumDownloadCountByStorageIdAndRepositoryId(storageIdAndRepositoryIdList);
        Long dependencyCount = sumDependencyCountByStorageIdsAndRepositoryIds(storageIdAndRepositoryIdList, null, null, null);
        Map<String, Long> map = Maps.newHashMap();
        map.put("downloadCount", downloadCount);
        map.put("dependencyCount", dependencyCount);
        return map;
    }

    private Long sumDownloadCountByStorageIdAndRepositoryId(List<String> storageIdAndRepositoryIdList) {
        EntityTraversal<Vertex, Vertex> entityTraversal = g().V().hasLabel(Vertices.ARTIFACT).has(Properties.DOWNLOAD_COUNT, P.gt(0));
        if (CollectionUtils.isNotEmpty(storageIdAndRepositoryIdList)) {
            entityTraversal = entityTraversal.has(Properties.STORAGE_ID_AND_REPOSITORY_ID, P.within(storageIdAndRepositoryIdList));
        }
        return entityTraversal.values(Properties.DOWNLOAD_COUNT).sum().tryNext().orElse(0L).longValue();
    }

    private EntityTraversal<Vertex, Vertex> commonBuildEntityTraversal(List<String> storageIdAndRepositoryIdList, String date, Long startDate, Long endDate) {
        EntityTraversal<Vertex, Vertex> entityTraversal = g().V().hasLabel(Vertices.ARTIFACT).has(Properties.STORAGE_ID_AND_REPOSITORY_ID, P.within(storageIdAndRepositoryIdList));
        if (StringUtils.isNotBlank(date)) {
            entityTraversal = entityTraversal.has(Properties.SCAN_DATE, date);
        }
        if (Objects.nonNull(startDate) && Objects.nonNull(endDate)) {
            entityTraversal = entityTraversal.has(Properties.SCAN_DATE_TIME, P.between(startDate, endDate));
        }
        return entityTraversal.has(Properties.SAFE_LEVEL, SafeLevelEnum.SCAN_COMPLETE.getLevel());
    }

    private Long sumDependencyCountByStorageIdsAndRepositoryIds(List<String> storageIdAndRepositoryIdList, String date, Long startDate, Long endDate) {
        EntityTraversal<Vertex, Vertex> entityTraversal = commonBuildEntityTraversal(storageIdAndRepositoryIdList, date, startDate, endDate);
        return entityTraversal.has(Properties.DEPENDENCY_COUNT, P.gt(0)).values(Properties.DEPENDENCY_COUNT).sum().tryNext().orElse(0L).longValue();
    }

    private Long sumDependencyVulnerabilitiesCountByStorageIdsAndRepositoryIds(List<String> storageIdAndRepositoryIdList, String date, Long startDate, Long endDate) {
        EntityTraversal<Vertex, Vertex> entityTraversal = commonBuildEntityTraversal(storageIdAndRepositoryIdList, date, startDate, endDate);
        return entityTraversal.has(Properties.DEPENDENCY_VULNERABILITIES_COUNT, P.gt(0)).values(Properties.DEPENDENCY_VULNERABILITIES_COUNT).sum().tryNext().orElse(0L).longValue();
    }

    private Long sumVulnerabilitiesCountByStorageIdsAndRepositoryIds(List<String> storageIdAndRepositoryIdList, String date, Long startDate, Long endDate) {
        EntityTraversal<Vertex, Vertex> entityTraversal = commonBuildEntityTraversal(storageIdAndRepositoryIdList, date, startDate, endDate);
        return entityTraversal.has(Properties.VULNERABILITIES_COUNT, P.gt(0)).values(Properties.VULNERABILITIES_COUNT).sum().tryNext().orElse(0L).longValue();
    }

    private Long sumSuppressedVulnerabilitiesCountByStorageIdsAndRepositoryIds(List<String> storageIdAndRepositoryIdList, String date, Long startDate, Long endDate) {
        EntityTraversal<Vertex, Vertex> entityTraversal = commonBuildEntityTraversal(storageIdAndRepositoryIdList, date, startDate, endDate);
        return entityTraversal.has(Properties.SUPPRESSED_VULNERABILITIES_COUNT, P.gt(0)).values(Properties.SUPPRESSED_VULNERABILITIES_COUNT).sum().tryNext().orElse(0L).longValue();
    }

    private Long scanCountByStorageIdsAndRepositoryIds(List<String> storageIdAndRepositoryIdList, String date, Long startDate, Long endDate) {
        EntityTraversal<Vertex, Vertex> entityTraversal = commonBuildEntityTraversal(storageIdAndRepositoryIdList, date, startDate, endDate);
        return entityTraversal.count().tryNext().orElse(0L);
    }

    private EntityTraversal<Vertex, Vertex> commonBuildEntityTraversalStorageAndRepository(List<String> storageIdAndRepositoryIdList) {
        return g().V().hasLabel(Vertices.ARTIFACT).has(Properties.STORAGE_ID_AND_REPOSITORY_ID, P.within(storageIdAndRepositoryIdList));
    }

    private Long unScanCountByStorageIdsAndRepositoryIds(List<String> storageIdAndRepositoryIdList) {
        return commonBuildEntityTraversalStorageAndRepository(storageIdAndRepositoryIdList).has(Properties.SAFE_LEVEL, SafeLevelEnum.UN_SCAN.getLevel()).count().tryNext().orElse(0L);
    }

    private Long notScanCountByStorageIdsAndRepositoryIds(List<String> storageIdAndRepositoryIdList) {
        return commonBuildEntityTraversalStorageAndRepository(storageIdAndRepositoryIdList).has(Properties.SAFE_LEVEL, SafeLevelEnum.UN_SCAN.getLevel()).count().tryNext().orElse(0L);
    }

    private Long scanSuccessCountByStorageIdsAndRepositoryIds(List<String> storageIdAndRepositoryIdList) {
        return commonBuildEntityTraversalStorageAndRepository(storageIdAndRepositoryIdList).has(Properties.SAFE_LEVEL, SafeLevelEnum.SCAN_COMPLETE.getLevel()).count().tryNext().orElse(0L);
    }

    private Long scanFailCountByStorageIdsAndRepositoryIds(List<String> storageIdAndRepositoryIdList) {
        return commonBuildEntityTraversalStorageAndRepository(storageIdAndRepositoryIdList).has(Properties.SAFE_LEVEL, SafeLevelEnum.SCAN_FAIL.getLevel()).count().tryNext().orElse(0L);
    }

    public Set<Vulnerability> fetchVulnerabilitiesByKeywords(String storageId, String repositoryId, String keywords) {
        Set<Vulnerability> vulnerabilitySet = Sets.newHashSet();
        String storageIdAndRepositoryId = String.format("%s-%s", storageId, repositoryId);
        EntityTraversal<Vertex, Vertex> entityTraversal = g().V().hasLabel(Vertices.ARTIFACT).has(Properties.STORAGE_ID_AND_REPOSITORY_ID, storageIdAndRepositoryId).has(Properties.UUID, Text.textContains(keywords));
        com.veadan.folib.storage.repository.Repository repository = configurationManager.getRepository(storageId, repositoryId);
        List<Artifact> artifactList = entityTraversal.map(artifactAdapter.fold(Optional.ofNullable(repository)
                .map(com.veadan.folib.storage.repository.Repository::getLayout)
                .map(ArtifactLayoutLocator.getLayoutByNameEntityMap()::get)
                .map(ArtifactLayoutDescription::getArtifactCoordinatesClass))).toList();
        if (CollectionUtils.isNotEmpty(artifactList)) {
            for (Artifact artifact : artifactList) {
                if (CollectionUtils.isNotEmpty(artifact.getVulnerabilitySet())) {
                    vulnerabilitySet.addAll(artifact.getVulnerabilitySet());
                }
            }
        }
        return vulnerabilitySet;
    }

    public Map<String, Long> countArtifactByStorageIdsAndRepositories(List<String> storageIdAndRepositoryIdList, List<String> disableStorageIdAndRepositoryIdList) {
        Long scanCount = scanCountByStorageIdsAndRepositoryIds(storageIdAndRepositoryIdList, null, null, null);
        Long notScanCount = notScanCountByStorageIdsAndRepositoryIds(disableStorageIdAndRepositoryIdList);
        Long scanSuccessCount = scanSuccessCountByStorageIdsAndRepositoryIds(storageIdAndRepositoryIdList);
        Long unScanCount = unScanCountByStorageIdsAndRepositoryIds(storageIdAndRepositoryIdList);
        Long scanFailCount = scanFailCountByStorageIdsAndRepositoryIds(storageIdAndRepositoryIdList);
        Long dependencyCount = sumDependencyCountByStorageIdsAndRepositoryIds(storageIdAndRepositoryIdList, null, null, null);
        Long dependencyVulnerabilitiesCount = sumDependencyVulnerabilitiesCountByStorageIdsAndRepositoryIds(storageIdAndRepositoryIdList, null, null, null);
        Long vulnerabilitiesCount = sumVulnerabilitiesCountByStorageIdsAndRepositoryIds(storageIdAndRepositoryIdList, null, null, null);
        Long suppressedVulnerabilitiesCount = sumSuppressedVulnerabilitiesCountByStorageIdsAndRepositoryIds(storageIdAndRepositoryIdList, null, null, null);
        Map<String, Long> map = Maps.newHashMap();
        map.put("scanCount", scanCount);
        map.put("notScanCount", notScanCount);
        map.put("scanSuccessCount", scanSuccessCount);
        map.put("unScanCount", unScanCount);
        map.put("scanFailCount", scanFailCount);
        map.put("dependencyCount", dependencyCount);
        map.put("dependencyVulnerabilitiesCount", dependencyVulnerabilitiesCount);
        map.put("vulnerabilitiesCount", vulnerabilitiesCount);
        map.put("suppressedVulnerabilitiesCount", suppressedVulnerabilitiesCount);
        return map;
    }

    public Map<String, Long> countRepositoryArtifactByStorageIdAndRepositoryId(String storageId, String repositoryId) {
        List<String> storageIdAndRepositoryIdList = Collections.singletonList(String.format("%s-%s", storageId, repositoryId));
        Long scanCount = scanCountByStorageIdsAndRepositoryIds(storageIdAndRepositoryIdList, null, null, null);
        Long dependencyVulnerabilitiesCount = sumDependencyVulnerabilitiesCountByStorageIdsAndRepositoryIds(storageIdAndRepositoryIdList, null, null, null);
        Long vulnerabilitiesCount = sumVulnerabilitiesCountByStorageIdsAndRepositoryIds(storageIdAndRepositoryIdList, null, null, null);
        Long suppressedVulnerabilitiesCount = sumSuppressedVulnerabilitiesCountByStorageIdsAndRepositoryIds(storageIdAndRepositoryIdList, null, null, null);
        Long dependencyCount = sumDependencyCountByStorageIdsAndRepositoryIds(storageIdAndRepositoryIdList, null, null, null);
        Map<String, Long> map = Maps.newHashMap();
        map.put("scanCount", scanCount);
        map.put("dependencyCount", dependencyCount);
        map.put("dependencyVulnerabilitiesCount", dependencyVulnerabilitiesCount);
        map.put("vulnerabilitiesCount", vulnerabilitiesCount);
        map.put("suppressedVulnerabilitiesCount", suppressedVulnerabilitiesCount);
        return map;
    }

    public Map<String, Long> countArtifactByStorageIdsAndRepositoryIdsAndDate(List<String> storageIdAndRepositoryIdList, String date, Long startDate, Long endDate) {
        Number dependencyCount = sumDependencyCountByStorageIdsAndRepositoryIds(storageIdAndRepositoryIdList, date, startDate, endDate);
        Number vulnerabilitiesCount = sumVulnerabilitiesCountByStorageIdsAndRepositoryIds(storageIdAndRepositoryIdList, date, startDate, endDate);
        Map<String, Long> map = Maps.newHashMap();
        map.put("dependencyCount", dependencyCount.longValue());
        map.put("vulnerabilitiesCount", vulnerabilitiesCount.longValue());
        return map;
    }

    public Map<String, Long> countFullArtifactByStorageIdsAndRepositoryIdsAndDate(List<String> storageIdAndRepositoryIdList, Long startDate, Long endDate) {
        Long scanCount = scanCountByStorageIdsAndRepositoryIds(storageIdAndRepositoryIdList, null, startDate, endDate);
        Long dependencyCount = sumDependencyCountByStorageIdsAndRepositoryIds(storageIdAndRepositoryIdList, null, startDate, endDate);
        Long dependencyVulnerabilitiesCount = sumDependencyVulnerabilitiesCountByStorageIdsAndRepositoryIds(storageIdAndRepositoryIdList, null, startDate, endDate);
        Long vulnerabilitiesCount = sumVulnerabilitiesCountByStorageIdsAndRepositoryIds(storageIdAndRepositoryIdList, null, startDate, endDate);
        Long suppressedVulnerabilitiesCount = sumSuppressedVulnerabilitiesCountByStorageIdsAndRepositoryIds(storageIdAndRepositoryIdList, null, startDate, endDate);
        Map<String, Long> map = Maps.newHashMap();
        map.put("scanCount", scanCount);
        map.put("dependencyCount", dependencyCount);
        map.put("dependencyVulnerabilitiesCount", dependencyVulnerabilitiesCount);
        map.put("vulnerabilitiesCount", vulnerabilitiesCount);
        map.put("suppressedVulnerabilitiesCount", suppressedVulnerabilitiesCount);
        return map;
    }

    public List<VulnerabilityArtifactDomain> findMatchingHasVulnerabilityByStorageIdsAndLevels(List<String> storageIdList, Set<String> levels) {
        EntityTraversal<Vertex, Vertex> entityTraversal = g().V().hasLabel(Vertices.VULNERABILITY).has(Properties.VULNERABILITY_PLATFORM_NAME, P.within(VulnerabilityPlatformEnum.values()))
                .has(Properties.HIGHEST_SEVERITY_TEXT, P.within(levels)).as("v");
        if (CollectionUtils.isNotEmpty(storageIdList)) {
            entityTraversal = entityTraversal.inE(Edges.ARTIFACT_HAS_VULNERABILITIES).outV()
                    .has(Properties.STORAGE_ID, P.within(storageIdList));
        } else {
            entityTraversal = entityTraversal.inE(Edges.ARTIFACT_HAS_VULNERABILITIES).outV();
        }
        List<VulnerabilityArtifactDomain> artifactList = entityTraversal.map(artifactAdapter.vulnerabilityFold()).toList();
        return artifactList;
    }

    private EntityTraversal<Vertex, Vertex> buildEntityTraversal(Boolean regex, String artifactName,
                                                                 String metadataSearch,
                                                                 List<String> storageIdAndRepositoryIdList,
                                                                 String storageId,
                                                                 String repositoryId,
                                                                 List<String> repositoryIds,
                                                                 String beginDate,
                                                                 String endDate,
                                                                 String safeLevel,
                                                                 String sortField,
                                                                 String sortOrder) {
        EntityTraversal<Vertex, Vertex> entityTraversal = g().V().hasLabel(Vertices.ARTIFACT);
        if (StringUtils.isNotBlank(storageId)) {
            entityTraversal = entityTraversal.has(Properties.STORAGE_ID, storageId);
        }
        if (StringUtils.isNotBlank(repositoryId)) {
            entityTraversal = entityTraversal.has(Properties.REPOSITORY_ID, repositoryId);
        }
        if (CollectionUtils.isNotEmpty(repositoryIds)) {
            entityTraversal = entityTraversal.has(Properties.REPOSITORY_ID, P.within(repositoryIds));
        }
        if (CollectionUtils.isNotEmpty(storageIdAndRepositoryIdList)) {
            entityTraversal = entityTraversal.has(Properties.STORAGE_ID_AND_REPOSITORY_ID, P.within(storageIdAndRepositoryIdList));
        }
        if (StringUtils.isNotBlank(artifactName)) {
            if (Boolean.TRUE.equals(regex)) {
                entityTraversal = entityTraversal.has(Properties.UUID, Text.textRegex(artifactName));
                entityTraversal = entityTraversal.has(Properties.UUID, Text.textNotContains("blobs/sha256"));
                entityTraversal = entityTraversal.has(Properties.UUID, Text.textNotContains("manifest/sha256"));
            } else {
                entityTraversal = entityTraversal.has(Properties.UUID, Text.textContains(artifactName));
                entityTraversal = entityTraversal.has(Properties.UUID, Text.textNotContains("blobs/sha256"));
                entityTraversal = entityTraversal.has(Properties.UUID, Text.textNotContains("manifest/sha256"));
            }
        }
        if (StringUtils.isNotBlank(metadataSearch)) {
            entityTraversal = entityTraversal.has(Properties.METADATA, Text.textContains(metadataSearch));
        }
        if (StringUtils.isNotBlank(beginDate) && StringUtils.isNotBlank(endDate)) {
            LocalDateTime beginLocalDateTime = DateUtil.parseLocalDateTime(beginDate, DatePattern.NORM_DATETIME_MINUTE_PATTERN);
            LocalDateTime endLocalDateTime = DateUtil.parseLocalDateTime(endDate, DatePattern.NORM_DATETIME_MINUTE_PATTERN);
            Long begin = EntityTraversalUtils.toLong(beginLocalDateTime);
            Long end = EntityTraversalUtils.toLong(endLocalDateTime);
            entityTraversal = entityTraversal.has(Properties.CREATED, P.between(begin, end));
        } else {
            entityTraversal = entityTraversal.has(Properties.CREATED, P.gte(0));
        }
        if (StringUtils.isNotBlank(safeLevel)) {
            entityTraversal = entityTraversal.has(Properties.SAFE_LEVEL, safeLevel);
        }
        if (StringUtils.isNotBlank(sortField) && StringUtils.isNotBlank(sortOrder)) {
            entityTraversal = entityTraversal.order().by(sortField, Order.valueOf(sortOrder));
        }
        return entityTraversal;
    }

    private EntityTraversal<Vertex, Vertex> buildEntityTraversalByVulnerabilityUuid(String vulnerabilityUuid,
                                                                                    String storageId,
                                                                                    List<String> storageIdAndRepositoryIdList) {
        EntityTraversal<Vertex, Vertex> entityTraversal = g().V().hasLabel(Vertices.VULNERABILITY)
                .has(Properties.UUID, vulnerabilityUuid).inE(Edges.ARTIFACT_HAS_VULNERABILITIES).outV();
        if (StringUtils.isNotBlank(storageId)) {
            entityTraversal = entityTraversal.has(Properties.STORAGE_ID, storageId);
        }
        if (CollectionUtils.isNotEmpty(storageIdAndRepositoryIdList)) {
            entityTraversal = entityTraversal.has(Properties.STORAGE_ID_AND_REPOSITORY_ID, P.within(storageIdAndRepositoryIdList));
        }
        return entityTraversal;
    }

    public Page<Artifact> queryArtifactByVulnerabilityUuid(Pageable pagination, String vulnerabilityUuid, String artifactPath) {
        Long zero = 0L;
        Long count = buildEntityTraversalByVulnerabilityUuid(vulnerabilityUuid, artifactPath).count().tryNext().orElse(zero);
        if (zero.equals(count)) {
            return new PageImpl<>(Collections.emptyList(), pagination, count);
        }
        long low = pagination.getPageNumber() * pagination.getPageSize();
        long high = (pagination.getPageNumber() + 1) * pagination.getPageSize();
        List<Artifact> artifactList = buildEntityTraversalByVulnerabilityUuid(vulnerabilityUuid, artifactPath)
                .range(low, high)
                .map(artifactAdapter.fold()).toList();
        return new PageImpl<>(artifactList, pagination, count);
    }

    private EntityTraversal<Vertex, Vertex> buildEntityTraversalByVulnerabilityUuid(String vulnerabilityUuid,
                                                                                    String artifactPath) {
        EntityTraversal<Vertex, Vertex> entityTraversal = g().V().hasLabel(Vertices.VULNERABILITY)
                .has(Properties.UUID, vulnerabilityUuid).inE(Edges.ARTIFACT_HAS_VULNERABILITIES).outV();
        if (StringUtils.isNotBlank(artifactPath)) {
            entityTraversal = entityTraversal.has(Properties.UUID, Text.textContains(artifactPath));
        }
        return entityTraversal;
    }

    public Page<Artifact> queryArtifactByComponentUuid(Pageable pagination, String componentUuid, String artifactPath) {
        Long zero = 0L;
        Long count = buildEntityTraversalByComponentUuid(componentUuid, artifactPath).count().tryNext().orElse(zero);
        if (zero.equals(count)) {
            return new PageImpl<>(Collections.emptyList(), pagination, count);
        }
        long low = pagination.getPageNumber() * pagination.getPageSize();
        long high = (pagination.getPageNumber() + 1) * pagination.getPageSize();
        List<Artifact> artifactList = buildEntityTraversalByComponentUuid(componentUuid, artifactPath)
                .range(low, high)
                .map(artifactAdapter.fold()).toList();
        return new PageImpl<>(artifactList, pagination, count);
    }

    public List<Artifact> queryArtifactByComponentUuid(String componentUuid) {
        List<Artifact> artifactList = buildEntityTraversalByComponentUuid(componentUuid, "")
                .map(artifactAdapter.fold()).toList();
        return artifactList;
    }

    private EntityTraversal<Vertex, Vertex> buildEntityTraversalByComponentUuid(String componentUuid,
                                                                                String artifactPath) {
        EntityTraversal<Vertex, Vertex> entityTraversal = g().V().hasLabel(Vertices.COMPONENT)
                .has(Properties.UUID, componentUuid).inE(Edges.ARTIFACT_HAS_COMPONENTS).outV();
        if (StringUtils.isNotBlank(artifactPath)) {
            entityTraversal = entityTraversal.has(Properties.UUID, Text.textContains(artifactPath));
        }
        return entityTraversal;
    }

    public Boolean artifactExists(String storageId,
                                  String repositoryId,
                                  String path) {
        log.info("Find storageId [{}] repositoryId [{}] path [{}] artifactExists", storageId, repositoryId, path);
        long startTime = System.currentTimeMillis();
        EntityTraversal<Vertex, Vertex> t = g().V()
                .hasLabel(Vertices.GENERIC_ARTIFACT_COORDINATES)
                .has(Properties.UUID, path)
                .inE(Edges.ARTIFACT_HAS_ARTIFACT_COORDINATES)
                .otherV()
                .hasLabel(Vertices.ARTIFACT)
                .has(Properties.STORAGE_ID, storageId)
                .has(Properties.REPOSITORY_ID, repositoryId)
                .has(Properties.ARTIFACT_FILE_EXISTS, true);
        Boolean result = t.hasNext();
        log.info("Find storageId [{}] repositoryId [{}] path [{}] artifactExists [{}] take time [{}] ms", storageId, repositoryId, path, result, System.currentTimeMillis() - startTime);
        return result;
    }

    public Artifact findOneArtifact(String storageId,
                                    String repositoryId,
                                    String path) {
        log.info("FindOneArtifact storageId [{}] repositoryId [{}] path [{}]", storageId, repositoryId, path);
        com.veadan.folib.storage.repository.Repository repository = configurationManager.getRepository(storageId, repositoryId);
        long startTime = System.currentTimeMillis();
        EntityTraversal<Vertex, Artifact> t = g().V()
                .hasLabel(Vertices.ARTIFACT)
                .has(Properties.UUID, String.format("%s-%s-%s", storageId, repositoryId, path))
                .map(artifactAdapter.fold(Optional.ofNullable(repository)
                        .map(com.veadan.folib.storage.repository.Repository::getLayout)
                        .map(ArtifactLayoutLocator.getLayoutByNameEntityMap()::get)
                        .map(ArtifactLayoutDescription::getArtifactCoordinatesClass)));
        Artifact artifact = t.tryNext().orElse(null);
        log.info("FindOneArtifact storageId [{}] repositoryId [{}] path [{}] artifactExists [{}] take time [{}] ms", storageId, repositoryId, path, Objects.nonNull(artifact), System.currentTimeMillis() - startTime);
        return artifact;
    }

    public List<Artifact> findPromotionMatchingByIndex(List<String> safeLevelList, List<String> promotionStatusList) {
        return g().V().hasLabel(Vertices.ARTIFACT).has(Properties.ARTIFACT_FILE_EXISTS, true).has(Properties.SAFE_LEVEL, P.within(safeLevelList)).has(Properties.PROMOTION, P.within(promotionStatusList)).map(artifactAdapter.fold()).toList();
    }

}

@Repository
interface ArtifactEntityQueries extends org.springframework.data.repository.Repository<Artifact, String> {

    @Query("MATCH (genericCoordinates:GenericArtifactCoordinates)<-[r1]-(artifact:Artifact) " +
            "WHERE genericCoordinates.uuid=$path AND artifact.storageId=$storageId AND artifact.repositoryId=$repositoryId " +
            "WITH artifact, r1, genericCoordinates " +
            "OPTIONAL MATCH (artifact)-[r4]->(tag:ArtifactTag) " +
            "WITH artifact, r1, genericCoordinates, r4, tag " +
            "MATCH (genericCoordinates)<-[r2]-(layoutCoordinates) " +
            "WITH artifact, r1, genericCoordinates, r2, layoutCoordinates, r4, tag " +
            "RETURN artifact, r1, genericCoordinates, r2, layoutCoordinates, r4, tag")
    List<Artifact> findByPathLike(@Param("storageId") String storageId,
                                  @Param("repositoryId") String repositoryId,
                                  @Param("path") String path);

    @Query(value = "MATCH (genericCoordinates:GenericArtifactCoordinates)<-[r1]-(artifact:Artifact) " +
            "WHERE artifact.lastUsed <= coalesce($lastAccessedDate, artifact.lastUsed) AND artifact.sizeInBytes >=  coalesce($minSizeInBytes, artifact.sizeInBytes) " +
            "WITH artifact, r1, genericCoordinates " +
            "OPTIONAL MATCH (artifact)-[r4]->(tag:ArtifactTag) " +
            "WITH artifact, r1, genericCoordinates, r4, tag " +
            "MATCH (genericCoordinates)<-[r2]-(layoutCoordinates) " +
            "WITH artifact, r1, genericCoordinates, r2, layoutCoordinates, r4, tag " +
            "RETURN artifact, r1, genericCoordinates, r2, layoutCoordinates, r4, tag",
            countQuery = "MATCH (artifact:Artifact) " +
                    "WHERE artifact.lastUsed <= coalesce($lastAccessedDate, artifact.lastUsed) AND artifact.sizeInBytes >=  coalesce($minSizeInBytes, artifact.sizeInBytes) " +
                    "RETURN count(artifact)")
    Page<Artifact> findMatching(@Param("lastAccessedDate") LocalDateTime lastAccessedDate,
                                @Param("minSizeInBytes") Long minSizeInBytes,
                                Pageable page);

    @Query("MATCH (genericCoordinates:GenericArtifactCoordinates)<-[r1]-(artifact:Artifact) " +
            "WHERE genericCoordinates.uuid=$path AND artifact.storageId=$storageId AND artifact.repositoryId=$repositoryId " +
            "RETURN exists(artifact.uuid)")
    Boolean artifactEntityExists(@Param("storageId") String storageId,
                                 @Param("repositoryId") String repositoryId,
                                 @Param("path") String path);

    @Query(value = "MATCH (genericCoordinates:GenericArtifactCoordinates)<-[r1]-(artifact:Artifact) " +
            "WHERE artifact.uuid CONTAINS  $artifactName" +
            "WITH artifact, r1, genericCoordinates " +
            "OPTIONAL MATCH (artifact)-[r4]->(tag:ArtifactTag) " +
            "WITH artifact, r1, genericCoordinates, r4, tag " +
            "MATCH (genericCoordinates)<-[r2]-(layoutCoordinates) " +
            "WITH artifact, r1, genericCoordinates, r2, layoutCoordinates, r4, tag " +
            "RETURN artifact, r1, genericCoordinates, r2, layoutCoordinates, r4, tag",
            countQuery = "MATCH (artifact:Artifact) " +
                    "WHERE  artifact.uuid CONTAINS $artifactName" +
                    "RETURN count(artifact)")
    Page<Artifact> findMatching1(@Param("artifactName") String artifactName,
                                 Pageable page);

    @Query(value = "MATCH (genericCoordinates:GenericArtifactCoordinates)<-[r1]-(artifact:Artifact) " +
            "WHERE artifact.uuid CONTAINS  $artifactName AND artifact.storageId=$storageId AND artifact.repositoryId=$repositoryId " +
            "WITH artifact, r1, genericCoordinates " +
            "OPTIONAL MATCH (artifact)-[r4]->(tag:ArtifactTag) " +
            "WITH artifact, r1, genericCoordinates, r4, tag " +
            "MATCH (genericCoordinates)<-[r2]-(layoutCoordinates) " +
            "WITH artifact, r1, genericCoordinates, r2, layoutCoordinates, r4, tag " +
            "RETURN artifact, r1, genericCoordinates, r2, layoutCoordinates, r4, tag",
            countQuery = "MATCH (artifact:Artifact) " +
                    "WHERE  artifact.uuid CONTAINS $artifactName AND artifact.storageId=$storageId AND artifact.repositoryId=$repositoryId " +
                    "RETURN count(artifact)")
    Page<Artifact> findMatching2(@Param("artifactName") String artifactName,
                                 @Param("storageId") String storageId,
                                 @Param("repositoryId") String repositoryId,
                                 Pageable page);


}
