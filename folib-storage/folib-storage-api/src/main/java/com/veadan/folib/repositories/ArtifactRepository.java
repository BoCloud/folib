package com.veadan.folib.repositories;

import cn.hutool.core.date.DatePattern;
import cn.hutool.core.date.DateUtil;
import com.google.common.collect.Maps;
import com.google.common.collect.Sets;
import com.veadan.folib.artifact.coordinates.ArtifactLayoutDescription;
import com.veadan.folib.artifact.coordinates.ArtifactLayoutLocator;
import com.veadan.folib.components.DistributedLockComponent;
import com.veadan.folib.configuration.ConfigurationManager;
import com.veadan.folib.configuration.ConfigurationUtils;
import com.veadan.folib.constant.GlobalConstants;
import com.veadan.folib.db.schema.Edges;
import com.veadan.folib.db.schema.Properties;
import com.veadan.folib.db.schema.Vertices;
import com.veadan.folib.domain.*;
import com.veadan.folib.enums.ArtifactFieldTypeEnum;
import com.veadan.folib.enums.ArtifactSearchConditionTypeEnum;
import com.veadan.folib.enums.SafeLevelEnum;
import com.veadan.folib.enums.VulnerabilityPlatformEnum;
import com.veadan.folib.gremlin.adapters.ArtifactAdapter;
import com.veadan.folib.gremlin.dsl.EntityTraversal;
import com.veadan.folib.gremlin.dsl.EntityTraversalUtils;
import com.veadan.folib.gremlin.dsl.__;
import com.veadan.folib.gremlin.repositories.GremlinVertexRepository;
import com.veadan.folib.services.ConfigurationManagementService;
import com.veadan.folib.storage.repository.RepositoryTypeEnum;
import com.veadan.folib.util.CommonUtils;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.compress.utils.Lists;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.apache.tinkerpop.gremlin.process.traversal.Order;
import org.apache.tinkerpop.gremlin.process.traversal.P;
import org.apache.tinkerpop.gremlin.structure.Vertex;
import org.janusgraph.core.attribute.Text;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Repository;

import javax.inject.Inject;
import javax.transaction.Transactional;
import java.time.LocalDateTime;
import java.util.*;
import java.util.concurrent.TimeUnit;

@Slf4j
@Repository
@Transactional
public class ArtifactRepository extends GremlinVertexRepository<Artifact> {
    //查找标记
    @Inject
    ArtifactAdapter artifactAdapter;
    @Inject
    ConfigurationManager configurationManager;
    @Inject
    ConfigurationManagementService configurationManagementService;
    @Inject
    DistributedLockComponent distributedLockComponent;

    @Override
    protected ArtifactAdapter adapter() {
        return artifactAdapter;
    }

    public void saveOrUpdate(Artifact artifact) {
        if (distributedLockComponent.lock(artifact.getUuid(), GlobalConstants.WAIT_LOCK_TIME, TimeUnit.SECONDS)) {
            try {
                try {
                    merge(artifact);
                } catch (Exception ex) {
                    if (CommonUtils.catchException(ex)) {
                        log.warn("Handle artifact [{}] catch error", artifact.getUuid());
                        return;
                    }
                    log.error("Handle artifact [{}] error [{}]", artifact.getUuid(), ExceptionUtils.getStackTrace(ex));
                    throw new RuntimeException(ex.getMessage());
                }
            } finally {
                distributedLockComponent.unLock(artifact.getUuid());
            }
        } else {
            log.warn("Handle artifact [{}] was not get lock", artifact.getUuid());
        }
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
        Page<Artifact> result = new PageImpl<Artifact>(Collections.emptyList(), pagination, 0);

        return new PageImpl<>(EntityTraversalUtils.reduceHierarchy(result.toList()), pagination, result.getTotalElements());
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
                .map(artifactAdapter.searchFold(Optional.ofNullable(repository)
                        .map(com.veadan.folib.storage.repository.Repository::getLayout)
                        .map(ArtifactLayoutLocator.getLayoutByNameEntityMap()::get)
                        .map(ArtifactLayoutDescription::getArtifactCoordinatesClass))).toList();
        return new PageImpl<>(artifactList, pagination, count);
    }

    public ArtifactSearch<Artifact> findMatchingByAql(ArtifactPage pagination, ArtifactSearchCondition artifactSearchCondition) {
        com.veadan.folib.storage.repository.Repository repository = null;
        boolean isGroupRepository = false;
        String storageId = artifactSearchCondition.getStorageId(), repositoryId = artifactSearchCondition.getRepositoryId();
        List<String> storageIdAndRepositoryIdList = null;
        ArtifactSearchRange artifactSearchRange = ArtifactSearchRange.builder().startPos(pagination.getOffset()).limit(pagination.getLimit()).endPos(0L).total(0L).build();
        ArtifactSearch artifactSearch = ArtifactSearch.builder().build();
        artifactSearch.setRange(artifactSearchRange);
        artifactSearch.setResults(Collections.emptyList());
        if (StringUtils.isNotBlank(storageId) && StringUtils.isNotBlank(repositoryId)) {
            repository = configurationManager.getRepository(storageId, repositoryId);
            if (repository == null) {
                return artifactSearch;
            }
            isGroupRepository = RepositoryTypeEnum.GROUP.getType().equals(repository.getType());
            if (isGroupRepository) {
                storageIdAndRepositoryIdList = getGroupStorageIdAndRepositoryId(repository);
                storageId = "";
                repositoryId = "";
            }
        }
        Long zero = 0L;
        Long count = buildEntityTraversalByAql(storageId, repositoryId, storageIdAndRepositoryIdList, artifactSearchCondition).count().tryNext().orElse(zero);
        if (zero.equals(count)) {
            return artifactSearch;
        }
        long offset = pagination.getOffset();
        long limit = pagination.getLimit();
        count = count - offset;
        EntityTraversal<Vertex, Vertex> entityTraversal = buildEntityTraversalByAql(storageId, repositoryId, storageIdAndRepositoryIdList, artifactSearchCondition);
        if (CollectionUtils.isEmpty(artifactSearchCondition.getArtifactSorts())) {
            entityTraversal = entityTraversal.order().by(Properties.CREATED, Order.valueOf("desc"));
        } else {
            for (ArtifactSort artifactSort : artifactSearchCondition.getArtifactSorts()) {
                for (String key : artifactSort.getKeyList()) {
                    entityTraversal = entityTraversal.order().by(key, Order.valueOf(artifactSort.getOrder()));
                }
            }
        }
        artifactSearchRange.setEndPos(count);
        artifactSearchRange.setTotal(count);
        List<Artifact> artifactList = entityTraversal.skip(offset).limit(limit)
                .map(artifactAdapter.fold()).toList();
        artifactSearch.setResults(artifactList);
        return artifactSearch;
    }

    public Page<Artifact> findMatchingForThirdParty(Pageable pagination, String searchKeyword) {
        Long zero = 0L;
        Long count = buildEntityTraversalForThirdParty(searchKeyword).count().tryNext().orElse(zero);
        if (zero.equals(count)) {
            return new PageImpl<>(Collections.emptyList(), pagination, count);
        }
        long low = pagination.getPageNumber() * pagination.getPageSize();
        long high = (pagination.getPageNumber() + 1) * pagination.getPageSize();
        List<Artifact> artifactList = buildEntityTraversalForThirdParty(searchKeyword)
                .range(low, high)
                .map(artifactAdapter.fold()).toList();
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
        List<Artifact> artifactList = buildEntityTraversalByVulnerabilityUuid(vulnerabilityUuid, storageId, storageIdAndRepositoryIdList).range(0, 1000)
                .map(artifactAdapter.fold()).toList();
        return EntityTraversalUtils.reduceHierarchy(artifactList);
    }

    public long countByVulnerabilityUuid(String vulnerabilityUuid,
                                         String storageId,
                                         List<String> storageIdAndRepositoryIdList) {
        return buildEntityTraversalByVulnerabilityUuid(vulnerabilityUuid, storageId, storageIdAndRepositoryIdList).count().tryNext().orElse(0L);
    }

    public List<Artifact> findMatchingBySafeLevels(List<String> storageIdAndRepositoryIdList, List<String> safeLevels) {
        List<Artifact> artifactList = g().V().hasLabel(Vertices.ARTIFACT).has(Properties.STORAGE_ID_AND_REPOSITORY_ID, P.within(storageIdAndRepositoryIdList)).has(Properties.SAFE_LEVEL, P.within(safeLevels)).order().by(Properties.SAFE_LEVEL, Order.valueOf("desc")).range(0, 800).map(artifactAdapter.baseFold(Optional.empty())).toList();
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

    private EntityTraversal<Vertex, Vertex> buildEntityTraversalByStorageIdAndRepositoryId(String storageId, String repositoryId) {
        EntityTraversal<Vertex, Vertex> entityTraversal = g().V().hasLabel(Vertices.ARTIFACT).has(Properties.ARTIFACT_FILE_EXISTS, true);
        if (StringUtils.isNotBlank(storageId)) {
            entityTraversal = entityTraversal.has(Properties.STORAGE_ID, storageId);
        }
        if (StringUtils.isNotBlank(repositoryId)) {
            entityTraversal = entityTraversal.has(Properties.REPOSITORY_ID, repositoryId);
        }
        return entityTraversal;
    }

    public Long countAllByStorageIdAndRepositoryId(String storageId, String repositoryId) {
       return buildEntityTraversalByStorageIdAndRepositoryId(storageId, repositoryId).count().tryNext().orElse(0L);
    }

    public List<Artifact> findByStorageIdAndRepositoryId(String storageId, String repositoryId, Pageable pageable) {
        long low = pageable.getPageNumber() * pageable.getPageSize();
        long high = (pageable.getPageNumber() + 1) * pageable.getPageSize();
        return buildEntityTraversalByStorageIdAndRepositoryId(storageId, repositoryId)
                .range(low, high)
                .map(artifactAdapter.baseFold(Optional.empty())).toList();
    }

    public Long artifactsBytesStatistics(List<String> storageIdAndRepositoryIdList) {
        EntityTraversal<Vertex, Vertex> entityTraversal = g().V().hasLabel(Vertices.ARTIFACT).has(Properties.ARTIFACT_FILE_EXISTS, true);
        if (CollectionUtils.isNotEmpty(storageIdAndRepositoryIdList)) {
            entityTraversal = entityTraversal.has(Properties.STORAGE_ID_AND_REPOSITORY_ID, P.within(storageIdAndRepositoryIdList));
        }
        return entityTraversal.values(Properties.SIZE_IN_BYTES).sum().tryNext().orElse(0L).longValue();
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

    public EntityTraversal<Vertex, Vertex> buildEntityTraversalByAql(String storageId, String repositoryId, List<String> storageIdAndRepositoryIdList, ArtifactSearchCondition artifactSearchCondition) {
        EntityTraversal<Vertex, Vertex> entityTraversal = g().V().hasLabel(Vertices.ARTIFACT);
        entityTraversal = entityTraversal.has(Properties.ARTIFACT_PATH, Text.textNotContains("blobs/sha256"));
        entityTraversal = entityTraversal.has(Properties.ARTIFACT_PATH, Text.textNotContains("manifest/sha256"));
        entityTraversal = entityTraversal.has(Properties.ARTIFACT_FILE_EXISTS, true);
        if (StringUtils.isNotBlank(storageId)) {
            entityTraversal = entityTraversal.has(Properties.STORAGE_ID, storageId);
        }
        if (StringUtils.isNotBlank(repositoryId)) {
            entityTraversal = entityTraversal.has(Properties.REPOSITORY_ID, repositoryId);
        }
        if (CollectionUtils.isNotEmpty(storageIdAndRepositoryIdList)) {
            entityTraversal = entityTraversal.has(Properties.STORAGE_ID_AND_REPOSITORY_ID, P.within(storageIdAndRepositoryIdList));
        }
        String path = artifactSearchCondition.getPath(), point = ".", pathMatch = "/*", asterisk = "\\*";
        if (StringUtils.isNotBlank(path) && !point.equals(path)) {
            if (path.endsWith(pathMatch)) {
                path = path.replace(pathMatch, "/.*");
                entityTraversal = entityTraversal.has(Properties.ARTIFACT_PATH, Text.textRegex(path));
            } else {
                entityTraversal = entityTraversal.has(Properties.ARTIFACT_PATH, Text.textContains(path));
            }
        }
        for (ArtifactConditionGroup artifactConditionGroup : artifactSearchCondition.getArtifactConditionGroups()) {
            if (ArtifactSearchConditionTypeEnum.OR.equals(artifactConditionGroup.getArtifactSearchConditionTypeEnum())) {
                List<EntityTraversal<Vertex, Vertex>> orEntityTraversalList = Lists.newArrayList();
                List<ArtifactCondition> artifactConditions = artifactConditionGroup.getArtifactConditions();
                if (CollectionUtils.isNotEmpty(artifactConditions)) {
                    String searchValue = "";
                    for (ArtifactCondition artifactCondition : artifactConditions) {
                        searchValue = artifactCondition.getSearchValue().replaceAll(asterisk, ".*");
                        if (searchValue.endsWith(pathMatch)) {
                            searchValue = searchValue.replace(pathMatch, "/.*");
                        }
                        if (point.equals(searchValue)) {
                            continue;
                        }
                        if (ArtifactSearchConditionTypeEnum.MATCH.equals(artifactCondition.getArtifactSearchConditionTypeEnum())) {
                            if (Properties.ARTIFACT_PATH.equals(artifactCondition.getSearchKey())) {
                                orEntityTraversalList.add(__.has(artifactCondition.getSearchKey(), searchValue.equals(artifactCondition.getSearchValue()) ? Text.textPrefix(searchValue) : Text.textRegex(searchValue)));
                            } else {
                                orEntityTraversalList.add(__.has(artifactCondition.getSearchKey(), searchValue.equals(artifactCondition.getSearchValue()) ? Text.textContains(searchValue) : Text.textRegex(searchValue)));
                            }
                        } else if (ArtifactSearchConditionTypeEnum.N_MATCH.equals(artifactCondition.getArtifactSearchConditionTypeEnum())) {
                            if (Properties.ARTIFACT_PATH.equals(artifactCondition.getSearchKey())) {
                                orEntityTraversalList.add(__.has(artifactCondition.getSearchKey(), searchValue.equals(artifactCondition.getSearchValue()) ? Text.textNotPrefix(searchValue) : Text.textNotRegex(searchValue)));
                            } else {
                                orEntityTraversalList.add(__.has(artifactCondition.getSearchKey(), searchValue.equals(artifactCondition.getSearchValue()) ? Text.textNotContains(searchValue) : Text.textNotRegex(searchValue)));
                            }
                        } else if (ArtifactSearchConditionTypeEnum.EQ.equals(artifactCondition.getArtifactSearchConditionTypeEnum())) {
                            orEntityTraversalList.add(__.has(artifactCondition.getSearchKey(), searchValue));
                        } else if (ArtifactSearchConditionTypeEnum.NE.equals(artifactCondition.getArtifactSearchConditionTypeEnum())) {
                            orEntityTraversalList.add(__.has(artifactCondition.getSearchKey(), P.neq(searchValue)));
                        }
                    }
                }
                List<ArtifactNameCondition> artifactNameConditions = artifactConditionGroup.getArtifactNameConditions();
                if (CollectionUtils.isNotEmpty(artifactNameConditions)) {
                    String searchValue = "";
                    for (ArtifactNameCondition artifactNameCondition : artifactNameConditions) {
                        searchValue = artifactNameCondition.getSearchValue().replaceAll(asterisk, ".*");
                        if (ArtifactSearchConditionTypeEnum.MATCH.equals(artifactNameCondition.getArtifactSearchConditionTypeEnum())) {
                            orEntityTraversalList.add(__.has(Properties.ARTIFACT_NAME, searchValue.equals(artifactNameCondition.getSearchValue()) ? Text.textContains(searchValue) : Text.textRegex(searchValue)));
                        } else if (ArtifactSearchConditionTypeEnum.N_MATCH.equals(artifactNameCondition.getArtifactSearchConditionTypeEnum())) {
                            orEntityTraversalList.add(__.has(Properties.ARTIFACT_NAME, searchValue.equals(artifactNameCondition.getSearchValue()) ? Text.textNotContains(searchValue) : Text.textNotRegex(searchValue)));
                        } else if (ArtifactSearchConditionTypeEnum.EQ.equals(artifactNameCondition.getArtifactSearchConditionTypeEnum())) {
                            orEntityTraversalList.add(__.has(Properties.ARTIFACT_NAME, searchValue));
                        } else if (ArtifactSearchConditionTypeEnum.NE.equals(artifactNameCondition.getArtifactSearchConditionTypeEnum())) {
                            orEntityTraversalList.add(__.has(Properties.ARTIFACT_NAME, P.neq(searchValue)));
                        }
                    }
                }
                List<ArtifactMetadataCondition> artifactMetadataConditions = artifactConditionGroup.getArtifactMetadataConditions();
                if (CollectionUtils.isNotEmpty(artifactMetadataConditions)) {
                    String metadataValue = "", regex = "";
                    for (ArtifactMetadataCondition artifactMetadataCondition : artifactMetadataConditions) {
                        regex = ".*\\\"%s\\\":\\{[^}]*\\\"value\\\":\\\"%s\\\"[^}]*}.*";
                        metadataValue = artifactMetadataCondition.getMedataValue();
                        if (ArtifactSearchConditionTypeEnum.EQ.equals(artifactMetadataCondition.getArtifactSearchConditionTypeEnum())) {
                            regex = String.format(regex, artifactMetadataCondition.getMedataKey(), artifactMetadataCondition.getMedataValue());
                            orEntityTraversalList.add(__.has(Properties.METADATA, Text.textRegex(regex)));
                        } else if (ArtifactSearchConditionTypeEnum.NE.equals(artifactMetadataCondition.getArtifactSearchConditionTypeEnum())) {
                            regex = String.format(regex, artifactMetadataCondition.getMedataKey(), artifactMetadataCondition.getMedataValue());
                            orEntityTraversalList.add(__.has(Properties.METADATA, Text.textNotRegex(regex)));
                        } else if (ArtifactSearchConditionTypeEnum.MATCH.equals(artifactMetadataCondition.getArtifactSearchConditionTypeEnum())) {
                            metadataValue = metadataValue.replaceAll(asterisk, ".*");
                            regex = String.format(regex, artifactMetadataCondition.getMedataKey(), metadataValue);
                            orEntityTraversalList.add(__.has(Properties.METADATA, Text.textRegex(regex)));
                        } else if (ArtifactSearchConditionTypeEnum.N_MATCH.equals(artifactMetadataCondition.getArtifactSearchConditionTypeEnum())) {
                            metadataValue = metadataValue.replaceAll(asterisk, ".*");
                            regex = String.format(regex, artifactMetadataCondition.getMedataKey(), metadataValue);
                            orEntityTraversalList.add(__.has(Properties.METADATA, Text.textNotRegex(regex)));
                        }
                    }
                }
                if (CollectionUtils.isNotEmpty(orEntityTraversalList)) {
                    EntityTraversal[] orEntityTraversalArray = orEntityTraversalList.toArray(new EntityTraversal[orEntityTraversalList.size()]);
                    entityTraversal = entityTraversal.or(orEntityTraversalArray);
                }
            } else if (ArtifactSearchConditionTypeEnum.AND.equals(artifactConditionGroup.getArtifactSearchConditionTypeEnum())) {
                List<ArtifactCondition> artifactConditions = artifactConditionGroup.getArtifactConditions();
                if (CollectionUtils.isNotEmpty(artifactConditions)) {
                    String searchValue = "";
                    for (ArtifactCondition artifactCondition : artifactConditions) {
                        searchValue = artifactCondition.getSearchValue().replaceAll(asterisk, ".*");
                        if (searchValue.endsWith(pathMatch)) {
                            searchValue = searchValue.replace(pathMatch, "/.*");
                        }
                        if (point.equals(searchValue)) {
                            continue;
                        }
                        if (ArtifactSearchConditionTypeEnum.MATCH.equals(artifactCondition.getArtifactSearchConditionTypeEnum())) {
                            if (Properties.ARTIFACT_PATH.equals(artifactCondition.getSearchKey())) {
                                entityTraversal = entityTraversal.has(artifactCondition.getSearchKey(), searchValue.equals(artifactCondition.getSearchValue()) ? Text.textPrefix(searchValue) : Text.textRegex(searchValue));
                            } else {
                                entityTraversal = entityTraversal.has(artifactCondition.getSearchKey(), searchValue.equals(artifactCondition.getSearchValue()) ? Text.textContains(searchValue) : Text.textRegex(searchValue));
                            }
                        } else if (ArtifactSearchConditionTypeEnum.N_MATCH.equals(artifactCondition.getArtifactSearchConditionTypeEnum())) {
                            if (Properties.ARTIFACT_PATH.equals(artifactCondition.getSearchKey())) {
                                entityTraversal = entityTraversal.has(artifactCondition.getSearchKey(), searchValue.equals(artifactCondition.getSearchValue()) ? Text.textNotPrefix(searchValue) : Text.textNotRegex(searchValue));
                            } else {
                                entityTraversal = entityTraversal.has(artifactCondition.getSearchKey(), searchValue.equals(artifactCondition.getSearchValue()) ? Text.textNotContains(searchValue) : Text.textNotRegex(searchValue));
                            }
                        } else if (ArtifactSearchConditionTypeEnum.EQ.equals(artifactCondition.getArtifactSearchConditionTypeEnum())) {
                            entityTraversal = entityTraversal.has(artifactCondition.getSearchKey(), searchValue);
                        } else if (ArtifactSearchConditionTypeEnum.NE.equals(artifactCondition.getArtifactSearchConditionTypeEnum())) {
                            entityTraversal = entityTraversal.has(artifactCondition.getSearchKey(), P.neq(searchValue));
                        } else if (ArtifactSearchConditionTypeEnum.GTE.equals(artifactCondition.getArtifactSearchConditionTypeEnum())) {
                            ArtifactFieldTypeEnum artifactFieldTypeEnum = ArtifactFieldTypeEnum.queryTypeEnum(artifactCondition.getSearchKey());
                            if (Objects.nonNull(artifactFieldTypeEnum) && ArtifactFieldTypeEnum.CREATED.getType().equals(artifactFieldTypeEnum.getType())) {
                                LocalDateTime localDateTime = DateUtil.parseLocalDateTime(searchValue, DatePattern.NORM_DATE_PATTERN);
                                searchValue = EntityTraversalUtils.toLong(localDateTime) + "";
                            }
                            entityTraversal = entityTraversal.has(artifactCondition.getSearchKey(), P.gte(searchValue));
                        } else if (ArtifactSearchConditionTypeEnum.LTE.equals(artifactCondition.getArtifactSearchConditionTypeEnum())) {
                            ArtifactFieldTypeEnum artifactFieldTypeEnum = ArtifactFieldTypeEnum.queryTypeEnum(artifactCondition.getSearchKey());
                            if (Objects.nonNull(artifactFieldTypeEnum) && ArtifactFieldTypeEnum.CREATED.getType().equals(artifactFieldTypeEnum.getType())) {
                                LocalDateTime localDateTime = DateUtil.parseLocalDateTime(searchValue, DatePattern.NORM_DATE_PATTERN);
                                searchValue = EntityTraversalUtils.toLong(localDateTime) + "";
                            }
                            entityTraversal = entityTraversal.has(artifactCondition.getSearchKey(), P.lte(searchValue));
                        }
                    }
                }
                List<ArtifactNameCondition> artifactNameConditions = artifactConditionGroup.getArtifactNameConditions();
                if (CollectionUtils.isNotEmpty(artifactNameConditions)) {
                    String searchValue = "";
                    for (ArtifactNameCondition artifactNameCondition : artifactNameConditions) {
                        searchValue = artifactNameCondition.getSearchValue().replaceAll(asterisk, ".*");
                        if (ArtifactSearchConditionTypeEnum.MATCH.equals(artifactNameCondition.getArtifactSearchConditionTypeEnum())) {
                            entityTraversal = entityTraversal.has(Properties.ARTIFACT_NAME, searchValue.equals(artifactNameCondition.getSearchValue()) ? Text.textContains(searchValue) : Text.textRegex(searchValue));
                        } else if (ArtifactSearchConditionTypeEnum.N_MATCH.equals(artifactNameCondition.getArtifactSearchConditionTypeEnum())) {
                            entityTraversal = entityTraversal.has(Properties.ARTIFACT_NAME, searchValue.equals(artifactNameCondition.getSearchValue()) ? Text.textNotContains(searchValue) : Text.textNotRegex(searchValue));
                        } else if (ArtifactSearchConditionTypeEnum.EQ.equals(artifactNameCondition.getArtifactSearchConditionTypeEnum())) {
                            entityTraversal = entityTraversal.has(Properties.ARTIFACT_NAME, searchValue);
                        } else if (ArtifactSearchConditionTypeEnum.NE.equals(artifactNameCondition.getArtifactSearchConditionTypeEnum())) {
                            entityTraversal = entityTraversal.has(Properties.ARTIFACT_NAME, P.neq(searchValue));
                        }
                    }
                }
                List<ArtifactMetadataCondition> artifactMetadataConditions = artifactConditionGroup.getArtifactMetadataConditions();
                if (CollectionUtils.isNotEmpty(artifactMetadataConditions)) {
                    String metadataValue = "", regex = "";
                    for (ArtifactMetadataCondition artifactMetadataCondition : artifactMetadataConditions) {
                        regex = ".*\\\"%s\\\":\\{[^}]*\\\"value\\\":\\\"%s\\\"[^}]*}.*";
                        metadataValue = artifactMetadataCondition.getMedataValue();
                        if (ArtifactSearchConditionTypeEnum.EQ.equals(artifactMetadataCondition.getArtifactSearchConditionTypeEnum())) {
                            regex = String.format(regex, artifactMetadataCondition.getMedataKey(), artifactMetadataCondition.getMedataValue());
                            entityTraversal = entityTraversal.has(Properties.METADATA, Text.textRegex(regex));
                        } else if (ArtifactSearchConditionTypeEnum.NE.equals(artifactMetadataCondition.getArtifactSearchConditionTypeEnum())) {
                            regex = String.format(regex, artifactMetadataCondition.getMedataKey(), artifactMetadataCondition.getMedataValue());
                            entityTraversal = entityTraversal.has(Properties.METADATA, Text.textNotRegex(regex));
                        } else if (ArtifactSearchConditionTypeEnum.MATCH.equals(artifactMetadataCondition.getArtifactSearchConditionTypeEnum())) {
                            metadataValue = metadataValue.replaceAll(asterisk, ".*");
                            regex = String.format(regex, artifactMetadataCondition.getMedataKey(), metadataValue);
                            entityTraversal = entityTraversal.has(Properties.METADATA, Text.textRegex(regex));
                        } else if (ArtifactSearchConditionTypeEnum.N_MATCH.equals(artifactMetadataCondition.getArtifactSearchConditionTypeEnum())) {
                            metadataValue = metadataValue.replaceAll(asterisk, ".*");
                            regex = String.format(regex, artifactMetadataCondition.getMedataKey(), metadataValue);
                            entityTraversal = entityTraversal.has(Properties.METADATA, Text.textNotRegex(regex));
                        }
                    }
                }
            }
        }
        return entityTraversal;
    }

    public EntityTraversal<Vertex, Vertex> buildEntityTraversalForThirdParty(String searchKeyword) {
        EntityTraversal<Vertex, Vertex> entityTraversal = g().V().hasLabel(Vertices.ARTIFACT);
        entityTraversal = entityTraversal.has(Properties.ARTIFACT_PATH, Text.textNotContains("blobs/sha256"));
        entityTraversal = entityTraversal.has(Properties.ARTIFACT_PATH, Text.textNotContains("manifest/sha256"));
        entityTraversal = entityTraversal.has(Properties.ARTIFACT_FILE_EXISTS, true);
        if (StringUtils.isNotBlank(searchKeyword)) {
            entityTraversal = entityTraversal.has(Properties.ARTIFACT_PATH, Text.textContains(searchKeyword));
        }
        entityTraversal = entityTraversal.order().by(Properties.CREATED, Order.desc);
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
        List<Artifact> artifactList = buildEntityTraversalByComponentUuid(componentUuid, "").range(0, 1000)
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
        log.debug("Find storageId [{}] repositoryId [{}] path [{}] artifactExists", storageId, repositoryId, path);
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
        log.debug("Find storageId [{}] repositoryId [{}] path [{}] artifactExists [{}] take time [{}] ms", storageId, repositoryId, path, result, System.currentTimeMillis() - startTime);
        return result;
    }

    public Artifact findOneArtifact(String storageId,
                                    String repositoryId,
                                    String path) {
        log.debug("FindOneArtifact storageId [{}] repositoryId [{}] path [{}]", storageId, repositoryId, path);
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
        log.debug("FindOneArtifact storageId [{}] repositoryId [{}] path [{}] artifactExists [{}] take time [{}] ms", storageId, repositoryId, path, Objects.nonNull(artifact), System.currentTimeMillis() - startTime);
        return artifact;
    }

    public Artifact findArtifactReport(String storageId,
                                       String repositoryId,
                                       String path) {
        com.veadan.folib.storage.repository.Repository repository = configurationManager.getRepository(storageId, repositoryId);
        EntityTraversal<Vertex, Artifact> t = g().V()
                .hasLabel(Vertices.ARTIFACT)
                .has(Properties.UUID, String.format("%s-%s-%s", storageId, repositoryId, path))
                .map(artifactAdapter.reportFold(Optional.ofNullable(repository)
                        .map(com.veadan.folib.storage.repository.Repository::getLayout)
                        .map(ArtifactLayoutLocator.getLayoutByNameEntityMap()::get)
                        .map(ArtifactLayoutDescription::getArtifactCoordinatesClass)));
        return t.tryNext().orElse(null);
    }

    public Artifact findOneArtifactBase(String storageId,
                                        String repositoryId,
                                        String path) {
        com.veadan.folib.storage.repository.Repository repository = configurationManager.getRepository(storageId, repositoryId);
        EntityTraversal<Vertex, Artifact> t = g().V()
                .hasLabel(Vertices.ARTIFACT)
                .has(Properties.UUID, String.format("%s-%s-%s", storageId, repositoryId, path))
                .map(artifactAdapter.baseFold(Optional.ofNullable(repository)
                        .map(com.veadan.folib.storage.repository.Repository::getLayout)
                        .map(ArtifactLayoutLocator.getLayoutByNameEntityMap()::get)
                        .map(ArtifactLayoutDescription::getArtifactCoordinatesClass)));
        Artifact artifact = t.tryNext().orElse(null);
        return artifact;
    }

    public long countPromotionMatchingByIndex(List<String> safeLevelList, List<String> promotionStatusList) {
        return g().V().hasLabel(Vertices.ARTIFACT).has(Properties.ARTIFACT_FILE_EXISTS, true).has(Properties.SAFE_LEVEL, P.within(safeLevelList)).has(Properties.PROMOTION, P.within(promotionStatusList)).count().tryNext().orElse(0L);
    }

    public List<Artifact> findPromotionMatchingByIndex(List<String> safeLevelList, List<String> promotionStatusList, Pageable pageable) {
        long low = pageable.getPageNumber() * pageable.getPageSize();
        long high = (pageable.getPageNumber() + 1) * pageable.getPageSize();
        return g().V().hasLabel(Vertices.ARTIFACT).has(Properties.ARTIFACT_FILE_EXISTS, true).has(Properties.SAFE_LEVEL, P.within(safeLevelList)).has(Properties.PROMOTION, P.within(promotionStatusList)).range(low, high).map(artifactAdapter.fold()).toList();
    }

    public Long artifactsCount() {
        return g().V().hasLabel(Vertices.ARTIFACT).has(Properties.ARTIFACT_FILE_EXISTS, true).count().tryNext().orElse(0L);
    }

    public Long artifactsVulnerabilitiesCount() {
        return g().V().hasLabel(Vertices.ARTIFACT).has(Properties.ARTIFACT_FILE_EXISTS, true).has(Properties.VULNERABILITIES_COUNT, P.gt(0)).count().tryNext().orElse(0L);
    }

    public Long criticalVulnerabilitiesCount() {
        return g().V().hasLabel(Vertices.ARTIFACT).has(Properties.ARTIFACT_FILE_EXISTS, true).has(Properties.CRITICAL_VULNERABILITIES_COUNT, P.gt(0)).values(Properties.CRITICAL_VULNERABILITIES_COUNT).sum().tryNext().orElse(0L).longValue();
    }

    public Long highVulnerabilitiesCount() {
        return g().V().hasLabel(Vertices.ARTIFACT).has(Properties.ARTIFACT_FILE_EXISTS, true).has(Properties.HIGH_VULNERABILITIES_COUNT, P.gt(0)).values(Properties.HIGH_VULNERABILITIES_COUNT).sum().tryNext().orElse(0L).longValue();
    }

    public Long mediumVulnerabilitiesCount() {
        return g().V().hasLabel(Vertices.ARTIFACT).has(Properties.ARTIFACT_FILE_EXISTS, true).has(Properties.MEDIUM_VULNERABILITIES_COUNT, P.gt(0)).values(Properties.MEDIUM_VULNERABILITIES_COUNT).sum().tryNext().orElse(0L).longValue();
    }

    public Long lowVulnerabilitiesCount() {
        return g().V().hasLabel(Vertices.ARTIFACT).has(Properties.ARTIFACT_FILE_EXISTS, true).has(Properties.LOW_VULNERABILITIES_COUNT, P.gt(0)).values(Properties.LOW_VULNERABILITIES_COUNT).sum().tryNext().orElse(0L).longValue();
    }

    public Long suppressedVulnerabilitiesCount() {
        return g().V().hasLabel(Vertices.ARTIFACT).has(Properties.ARTIFACT_FILE_EXISTS, true).has(Properties.SUPPRESSED_VULNERABILITIES_COUNT, P.gt(0)).values(Properties.SUPPRESSED_VULNERABILITIES_COUNT).sum().tryNext().orElse(0L).longValue();
    }

    public Long artifactsCount(String storageId, String repositoryId) {
        return g().V().hasLabel(Vertices.ARTIFACT).has(Properties.CREATED, P.gt(0)).has(Properties.STORAGE_ID, storageId).has(Properties.REPOSITORY_ID, repositoryId).count().tryNext().orElse(0L);
    }

    public void dropArtifacts(String storageId, String repositoryId, Integer limit) {
        if (Objects.isNull(limit)) {
            limit = 800;
        }
        g().V().hasLabel(Vertices.ARTIFACT).has(Properties.CREATED, P.gt(0)).has(Properties.STORAGE_ID, storageId).has(Properties.REPOSITORY_ID, repositoryId).limit(limit).drop().iterate();
    }

    public Long countDockerArtifactRelation(String storageId, String repositoryId, String uuid, String keyword, Integer type) {
        EntityTraversal<Vertex, Vertex> entityTraversal = g().V().hasLabel(Vertices.ARTIFACT).has(Properties.STORAGE_ID, storageId).has(Properties.REPOSITORY_ID, repositoryId).has(Properties.UUID, Text.textNotPrefix(uuid));
        if (type == 1) {
            entityTraversal = entityTraversal.has(Properties.UUID, Text.textContains(keyword)).has(Properties.UUID, Text.textNotContains("blobs/sha256")).has(Properties.UUID, Text.textNotContains("manifest/sha256")).has(Properties.ARTIFACT_FILE_EXISTS, true);
        } else if (type == 2) {
            entityTraversal = entityTraversal.has(Properties.UUID, Text.textContains(keyword)).has(Properties.UUID, Text.textContains("manifest/sha256")).has(Properties.ARTIFACT_FILE_EXISTS, true);
        } else if (type == 3) {
            entityTraversal = entityTraversal.has(Properties.LAYERS, keyword).has(Properties.UUID, Text.textContains("manifest/sha256")).has(Properties.ARTIFACT_FILE_EXISTS, true);
        }
        long count = entityTraversal.count().tryNext().orElse(0L);
        log.debug("Count for [{}] [{}] [{}] [{}] [{}] [{}]", storageId, repositoryId, uuid, keyword, type, count);
        return count;
    }

}
