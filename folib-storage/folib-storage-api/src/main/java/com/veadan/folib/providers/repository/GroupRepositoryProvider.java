package com.veadan.folib.providers.repository;


import cn.hutool.core.collection.CollectionUtil;
import com.beust.jcommander.internal.Maps;
import com.veadan.folib.artifact.coordinates.ArtifactCoordinates;
import com.veadan.folib.configuration.ConfigurationUtils;
import com.veadan.folib.data.criteria.Paginator;
import com.veadan.folib.enums.ProductTypeEnum;
import com.veadan.folib.providers.io.AbstractRepositoryProvider;
import com.veadan.folib.providers.io.RepositoryFiles;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.providers.io.RepositoryPathResolver;
import com.veadan.folib.providers.repository.event.GroupRepositoryPathFetchEvent;
import com.veadan.folib.providers.repository.group.GroupRepositorySetCollector;
import com.veadan.folib.repositories.ArtifactIdGroupRepository;
import com.veadan.folib.services.ArtifactResolutionService;
import com.veadan.folib.services.support.ArtifactRoutingRulesChecker;
import com.veadan.folib.storage.Storage;
import com.veadan.folib.storage.metadata.MetadataHelper;
import com.veadan.folib.storage.repository.Repository;
import com.veadan.folib.util.ThrowingFunction;
import org.apache.commons.collections4.MapUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.apache.maven.index.artifact.M2ArtifactRecognizer;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Component;

import javax.inject.Inject;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.nio.file.Path;
import java.util.*;
import java.util.stream.Collectors;

/**
 * @author Veadan
 */
@Component
public class GroupRepositoryProvider
        extends AbstractRepositoryProvider {

    private static final Logger logger = LoggerFactory.getLogger(GroupRepositoryProvider.class);

    private static final String ALIAS = "group";

    @Inject
    private ArtifactRoutingRulesChecker artifactRoutingRulesChecker;

    @Inject
    private HostedRepositoryProvider hostedRepositoryProvider;

    @Inject
    private GroupRepositorySetCollector groupRepositorySetCollector;

    @Inject
    private ArtifactIdGroupRepository artifactIdGroupRepository;

    @Inject
    private RepositoryPathResolver repositoryPathResolver;

    @Inject
    private ArtifactResolutionService artifactResolutionService;

    @Override
    public String getAlias() {
        return ALIAS;
    }

    @Override
    protected InputStream getInputStreamInternal(RepositoryPath path)
            throws IOException {
        return hostedRepositoryProvider.getInputStreamInternal(path);
    }

    @Override
    public RepositoryPath fetchPath(RepositoryPath repositoryPath)
            throws IOException {
        if (Objects.isNull(repositoryPath)) {
            return null;
        }
        boolean mavenMetadata = ProductTypeEnum.Maven.getFoLibraryName().equals(repositoryPath.getRepository().getLayout()) &&
                MetadataHelper.MAVEN_METADATA_XML.equals(repositoryPath.getFileName().toString()) &&
                !M2ArtifactRecognizer.isSnapshot(repositoryPath.getParent().getFileName().toString());
        if (mavenMetadata) {
            eventPublisher.publishEvent(new GroupRepositoryPathFetchEvent(repositoryPath));
        }
        if (repositoryPath.toString().endsWith(MetadataHelper.MAVEN_METADATA_XML)) {
            RepositoryPath result = resolvePathDirectlyFromGroupPathIfPossible(repositoryPath);
            if (result != null) {
                return result;
            }
        }
        return resolvePathTraversal(repositoryPath);
    }

    protected RepositoryPath resolvePathTraversal(RepositoryPath repositoryPath)
            throws IOException {
        Repository groupRepository = repositoryPath.getRepository();
        Storage storage = groupRepository.getStorage();

        // Iterate over the `repositories` collection.
        RepositoryPath subRepositoryPath = null;
        for (String storageAndRepositoryId : groupRepository.getGroupRepositories()) {
            try {
                String sId = ConfigurationUtils.getStorageId(storage.getId(), storageAndRepositoryId);
                String rId = ConfigurationUtils.getRepositoryId(storageAndRepositoryId);

                Repository subRepository = getConfiguration().getStorage(sId).getRepository(rId);

                subRepositoryPath = repositoryPathResolver.resolve(subRepository, repositoryPath);
                if (!isRepositoryResolvable(groupRepository, subRepository, subRepositoryPath)) {
                    continue;
                }
                if (Objects.nonNull(repositoryPath.getEnableRemoteUrlPrefix())) {
                    subRepositoryPath.setEnableRemoteUrlPrefix(repositoryPath.getEnableRemoteUrlPrefix());
                }
                if (StringUtils.isNotBlank(repositoryPath.getTargetUrl())) {
                    subRepositoryPath.setTargetUrl(repositoryPath.getTargetUrl());
                }
                if (MapUtils.isNotEmpty(repositoryPath.getHeaders())) {
                    subRepositoryPath.setHeaders(repositoryPath.getHeaders());
                }
                if (StringUtils.isNotBlank(repositoryPath.getArtifactPath())) {
                    subRepositoryPath.setArtifactPath(repositoryPath.getArtifactPath());
                }
                subRepositoryPath = resolvePathFromGroupMemberOrTraverse(subRepositoryPath);
                if (subRepositoryPath == null) {
                    continue;
                }
                logger.info("Located artifact: [{}]", subRepositoryPath);
            } catch (Exception ex) {
                logger.error("group repository resolvePathTraversal artifact: [{}] error：[{}]", subRepositoryPath, ExceptionUtils.getStackTrace(ex));
            }
            if (Objects.nonNull(subRepositoryPath) && Objects.nonNull(resolvePathDirectlyFromGroupPathIfPossible(subRepositoryPath))) {
                return subRepositoryPath;
            }
        }

        return null;
    }

    private boolean isRepositoryResolvable(Repository groupRepository,
                                           Repository subRepository,
                                           RepositoryPath repositoryPath)
            throws IOException {
        final boolean isInService = subRepository.isInService();

        if (!isInService) {
            logger.info("- Repository [{}] is not in service, skipping...",
                    subRepository.getStorageIdAndRepositoryId());

            // early break to avoid wasting time on looping through the routing rules.
            return false;
        }

        final boolean isRoutable = !artifactRoutingRulesChecker.isDenied(groupRepository, repositoryPath);

        if (!isRoutable) {
            logger.info("- Repository [{}] is denied by a routing rule, skipping...",
                    subRepository.getStorageIdAndRepositoryId());
        }

        return isInService && isRoutable;
    }

    private RepositoryPath resolvePathDirectlyFromGroupPathIfPossible(final RepositoryPath artifactPath) {
        try {
            return hostedRepositoryProvider.fetchPath(artifactPath);
        } catch (Exception ex) {
            logger.warn(ExceptionUtils.getStackTrace(ex));
        }
        return null;
    }

    protected RepositoryPath resolvePathFromGroupMemberOrTraverse(RepositoryPath repositoryPath)
            throws IOException {
        Repository repository = repositoryPath.getRepository();
        if (getAlias().equals(repository.getType())) {
            return resolvePathTraversal(repositoryPath);
        }

        RepositoryProvider provider = repositoryProviderRegistry.getProvider(repository.getType());
        try {
            return (RepositoryPath) provider.fetchPath(repositoryPath);
        } catch (IOException e) {
            logger.error("Failed to resolve path [{}]", repositoryPath);
            return null;
        }
    }

    @Override
    protected OutputStream getOutputStreamInternal(RepositoryPath repositoryPath) {
        // It should not be possible to write artifacts to a group repository.
        // A group repository should only serve artifacts that already exist
        // in the repositories within the group.

        throw new UnsupportedOperationException();
    }

    @Override
    public List<Path> search(String storageId,
                             String repositoryId,
                             RepositorySearchRequest predicate,
                             Paginator paginator) {
        logger.info("Search in [{}]:[{}] ...", storageId, repositoryId);

        Map<ArtifactCoordinates, Path> resultMap = new LinkedHashMap<>();

        Storage storage = getConfiguration().getStorage(storageId);
        Repository groupRepository = storage.getRepository(repositoryId);
        Set<Repository> groupRepositorySet = groupRepositorySetCollector.collect(groupRepository);

        if (groupRepositorySet.isEmpty()) {
            return new LinkedList<>();
        }

        long skip = paginator.getSkip();
        int limit = paginator.getLimit();

        int groupSize = groupRepositorySet.size();
        long groupSkip = (skip / (limit * groupSize)) * limit;
        int groupLimit = limit;

        skip = skip - groupSkip;

        outer:
        do {
            Paginator paginatorLocal = new Paginator();
            paginatorLocal.setLimit(groupLimit);
            paginatorLocal.setSkip(groupSkip);
            paginatorLocal.setProperty(paginator.getProperty());
            paginatorLocal.setOrder(paginator.getOrder());
            paginatorLocal.setUseLimit(paginator.getUseLimit());
            groupLimit = 0;

            for (Iterator<Repository> i = groupRepositorySet.iterator(); i.hasNext(); ) {
                Repository r = i.next();
                RepositoryProvider repositoryProvider = repositoryProviderRegistry.getProvider(r.getType());

                List<Path> repositoryResult = repositoryProvider.search(r.getStorage().getId(), r.getId(), predicate,
                        paginatorLocal);
                if (repositoryResult.isEmpty()) {
                    i.remove();
                    continue;
                }

                // count coordinates intersection
                groupLimit += repositoryResult.stream()
                        .map(ThrowingFunction.unchecked((Path p) -> resultMap.put(getArtifactCoordinates(p), p)))
                        .filter(p -> p != null)
                        .collect(Collectors.toList())
                        .size();

                //Break search iterations if we have reached enough list size.
                if (resultMap.size() >= limit + skip) {
                    break outer;
                }
            }
            groupSkip += limit;

            // Will iterate until there is no more coordinates intersection and
            // there is more search results within group repositories
        }
        while (groupLimit > 0 && !groupRepositorySet.isEmpty());

        LinkedList<Path> resultList = new LinkedList<>();
        if (skip >= resultMap.size()) {
            return resultList;
        }
        resultList.addAll(resultMap.values());

        long toIndex = resultList.size() - skip > limit ? limit + skip : resultList.size();
        return resultList.subList((int) skip, (int) toIndex);
    }

    private ArtifactCoordinates getArtifactCoordinates(Path p) throws IOException {
        return RepositoryFiles.readCoordinates((RepositoryPath) p);
    }

    @Override
    public Long count(String storageId,
                      String repositoryId,
                      RepositorySearchRequest predicate) {
        logger.info("Count in [{}]:[{}] ...", storageId, repositoryId);

        Storage storage = getConfiguration().getStorage(storageId);
        Repository groupRepository = storage.getRepository(repositoryId);
        Set<Repository> groupRepositorySet = groupRepositorySetCollector.collect(groupRepository);
        Long count = 0L;
        if (groupRepositorySet.isEmpty()) {
            return count;
        }
        for (Iterator<Repository> i = groupRepositorySet.iterator(); i.hasNext(); ) {
            Repository r = i.next();
            RepositoryProvider repositoryProvider = repositoryProviderRegistry.getProvider(r.getType());
            count = count + repositoryProvider.count(r.getStorage().getId(), r.getId(), predicate);
        }
        return count;
    }

    @Override
    public Map<String, Object> searchConanPackage(Repository repository, String query) {
        Set<Repository> groupRepositorySet = groupRepositorySetCollector.collect(repository);
        for (Repository x : groupRepositorySet) {
            try {
                RepositoryProvider repositoryProvider = repositoryProviderRegistry.getProvider(x.getType());
                Map<String, Object> rsMap = repositoryProvider.searchConanPackage(x, query);
                if (rsMap.get("results") instanceof List) {
                    List pacakges = (List) rsMap.get("results");
                    if (CollectionUtil.isNotEmpty(pacakges)) {
                        return rsMap;
                    }
                }
            } catch (Exception e) {
                e.printStackTrace();
            }
        }
        return Maps.newHashMap();
    }

    @Override
    public ResponseEntity searchConanDownLoadUrl(Repository repository, String name, String version, String user, String channel) {
        Set<Repository> groupRepositorySet = groupRepositorySetCollector.collect(repository);
        for (Repository x : groupRepositorySet) {
            try {
                RepositoryProvider repositoryProvider = repositoryProviderRegistry.getProvider(x.getType());
                ResponseEntity responseEntity = repositoryProvider.searchConanDownLoadUrl(x, name, version, user, channel);
                if (HttpStatus.NOT_FOUND.value() != responseEntity.getStatusCode().value()) {
                    return responseEntity;
                }
            } catch (Exception e) {
                e.printStackTrace();
            }
        }
        return ResponseEntity.status(HttpStatus.NOT_FOUND).build();
    }

    @Override
    public Map<String, Object> searchConanPackageInfo(Repository repository, String name, String version, String user, String channel) {

        Set<Repository> groupRepositorySet = groupRepositorySetCollector.collect(repository);
        for (Repository x : groupRepositorySet) {
            try {
                RepositoryProvider repositoryProvider = repositoryProviderRegistry.getProvider(x.getType());
                Map<String, Object> rsMap = repositoryProvider.searchConanPackageInfo(x, name, version, user, channel);
                if (CollectionUtil.isNotEmpty(rsMap)) {
                    return rsMap;
                }
            } catch (Exception e) {
                e.printStackTrace();
            }
        }
        return Maps.newHashMap();
    }

}
