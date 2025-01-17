package com.veadan.folib.providers.repository;

import com.alibaba.fastjson.JSONObject;
import com.veadan.folib.artifact.ArtifactNotFoundException;
import com.veadan.folib.constant.GlobalConstants;
import com.veadan.folib.data.criteria.Paginator;
import com.veadan.folib.domain.Artifact;
import com.veadan.folib.enums.ProductTypeEnum;
import com.veadan.folib.providers.io.*;
import com.veadan.folib.repositories.ArtifactIdGroupRepository;
import com.veadan.folib.services.ArtifactManagementService;
import com.veadan.folib.storage.Storage;
import com.veadan.folib.storage.repository.Repository;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.compress.utils.Lists;
import org.apache.commons.io.FileUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.apache.maven.artifact.versioning.ComparableVersion;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

import javax.inject.Inject;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.lang.reflect.Type;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Stream;

/**
 * @author Veadan
 */
@Component
public class HostedRepositoryProvider extends AbstractRepositoryProvider {

    private static final Logger logger = LoggerFactory.getLogger(HostedRepositoryProvider.class);

    private static final String ALIAS = "hosted";

    @Inject
    private ArtifactIdGroupRepository artifactIdGroupRepository;

    @Inject
    private RepositoryPathResolver repositoryPathResolver;

    @Inject
    protected ArtifactManagementService artifactManagementService;

    @Override
    public String getAlias() {
        return ALIAS;
    }

    @Override
    protected InputStream getInputStreamInternal(RepositoryPath repositoryPath) throws IOException {
        try {
            return Files.newInputStream(repositoryPath);
        } catch (ArtifactNotFoundException e) {
            logger.info("The path [{}] does not exist!\n*\t[{}]", repositoryPath, e.getMessage());

            return null;
        } catch (IOException ex) {
            logger.error("Failed to decorate InputStream for [{}]", repositoryPath, ex);

            throw ex;
        }
    }

    @Override
    public OutputStream getOutputStreamInternal(RepositoryPath repositoryPath)
            throws IOException {
        return Files.newOutputStream(repositoryPath);
    }

    @Override
    public List<Path> search(String storageId,
                             String repositoryId,
                             RepositorySearchRequest predicate,
                             Paginator paginator) {
        List<Path> result = new LinkedList<Path>();

        Storage storage = configurationManager.getConfiguration().getStorage(storageId);
        Repository repository = storage.getRepository(repositoryId);

        RootRepositoryPath rootRepositoryPath = repositoryPathResolver.resolve(repository);
        long startTime = System.currentTimeMillis();
        List<Artifact> searchResult = artifactIdGroupRepository.findArtifactsGremlin(storageId, repositoryId, predicate.getArtifactId(),
                predicate.getUseArtifactName(), predicate.getCoordinateValues(), paginator.getSkip(), paginator.getLimit(), paginator.getUseLimit());
        logger.info("FindArtifacts storageId [{}] repositoryId [{}] artifactId [{}] coordinateValues [{}] skip [{}] limit [{}] useLimit [{}] artifactListSize [{}] take time [{}] ms", storageId, repositoryId, predicate.getArtifactId(), predicate.getCoordinateValues(), paginator.getSkip(), paginator.getLimit(), paginator.getUseLimit(), searchResult.size(), System.currentTimeMillis() - startTime);
        for (Artifact artifactEntry : searchResult) {

            try {
                result.add(rootRepositoryPath.resolve(artifactEntry));
            } catch (Exception e) {
                logger.error("Failed to resolve Artifact [{}]",
                        artifactEntry.getArtifactCoordinates(), e);
                continue;
            }
        }
        return result;
    }

    @Override
    public Long count(String storageId,
                      String repositoryId,
                      RepositorySearchRequest predicate) {
        return artifactIdGroupRepository.commonCountArtifacts(storageId, repositoryId, predicate.getArtifactId(),
                predicate.getUseArtifactName(), predicate.getCoordinateValues());
    }

    @Override
    protected RepositoryPath fetchPath(RepositoryPath repositoryPath)
            throws IOException {
        logger.debug(" -> Checking local cache for {} ...", repositoryPath);
        repositoryPath = resolveRealPath(repositoryPath);
        if (artifactNotExists(repositoryPath)) {
            logger.info("The artifact {} was not found in the local cache", repositoryPath);
            return null;
        }
        logger.debug("The artifact {} was found in the local cache", repositoryPath);
        return repositoryPath;
    }

    private boolean artifactNotExists(RepositoryPath repositoryPath) throws IOException {
        return !RepositoryFiles.artifactExists(repositoryPath);
    }

    private RepositoryPath resolveRealPath(RepositoryPath repositoryPath) {
        try {
            Repository repository = repositoryPath.getRepository();
            if (!ProductTypeEnum.Raw.getFoLibraryName().equals(repository.getLayout()) || Boolean.FALSE.equals(repository.getEnableCustomLayout()) || StringUtils.isBlank(repository.getCustomLayout())) {
                return repositoryPath;
            }
            String artifactPath = RepositoryFiles.relativizePath(repositoryPath);
            if (GlobalConstants.LATEST_ARTIFACT_KEY_LIST.stream().noneMatch(artifactPath::contains)) {
                return repositoryPath;
            }
            final Pattern pattern = Pattern.compile(repository.getCustomLayout());
            RepositoryPath repositoryParentPath = getParentPath(pattern, repositoryPath);
            if (Objects.isNull(repositoryParentPath) || !Files.exists(repositoryParentPath)) {
                return repositoryPath;
            }
            List<RepositoryPath> repositoryPathList = Lists.newArrayList();
            try (Stream<Path> pathStream = Files.walk(repositoryParentPath)) {
                pathStream.forEach(p -> {
                    try {
                        RepositoryPath matcherRepositoryPath = matcherPath(pattern, p);
                        if (Objects.nonNull(matcherRepositoryPath) && !repositoryPathList.contains(matcherRepositoryPath)) {
                            repositoryPathList.add(matcherRepositoryPath);
                            repositoryPathList.sort(Comparator.comparing(o -> new ComparableVersion(o.getFileName().toString())));
                        }
                    } catch (Exception ex) {
                        logger.error("Handler repositoryPath list error [{}]", ExceptionUtils.getStackTrace(ex));
                    }
                });
            }
            if (CollectionUtils.isNotEmpty(repositoryPathList)) {
                if (artifactPath.contains(GlobalConstants.RELEASE_ARTIFACT_KEY)) {
                    return releasePath(repositoryPathList);
                } else {
                    return repositoryPathList.get(repositoryPathList.size() - 1);
                }
            }
        } catch (Exception ex) {
            logger.error(ExceptionUtils.getStackTrace(ex));
        }
        return repositoryPath;
    }

    private RepositoryPath getParentPath(Pattern pattern, RepositoryPath repositoryPath) {
        try {
            if (RepositoryFiles.isTrash(repositoryPath) || RepositoryFiles.isHidden(repositoryPath) || !RepositoryFiles.isArtifact(repositoryPath)) {
                return null;
            }
            String artifactPath = RepositoryFiles.relativizePath(repositoryPath);
            Matcher matcher = pattern.matcher(artifactPath);
            if (matcher.matches()) {
                String orgPath = matcher.group("orgPath");
                String module = matcher.group("module");
                RootRepositoryPath rootRepositoryPath = repositoryPath.getFileSystem().getRootDirectory();
                return rootRepositoryPath.resolve(orgPath).resolve(module);
            }
            return null;
        } catch (Exception ex) {
            logger.error("Path [{}] getParentPath error [{}]", repositoryPath.toString(), ExceptionUtils.getStackTrace(ex));
        }
        return null;
    }

    private RepositoryPath matcherPath(Pattern pattern, Path path) {
        try {
            RepositoryPath repositoryPath = (RepositoryPath) path;
            if (RepositoryFiles.isTrash(repositoryPath) || RepositoryFiles.isHidden(repositoryPath) || !RepositoryFiles.isArtifact(repositoryPath)) {
                return null;
            }
            String artifactPath = RepositoryFiles.relativizePath(repositoryPath);
            Matcher matcher = pattern.matcher(artifactPath);
            if (matcher.matches()) {
                return repositoryPath;
            }
            return null;
        } catch (Exception ex) {
            logger.error("Path [{}] isMatcher error [{}]", path.toString(), ExceptionUtils.getStackTrace(ex));
        }
        return null;
    }

    private RepositoryPath releasePath(List<RepositoryPath> repositoryPathList) throws IOException {
        Map<String, RepositoryPath> releaseMap = new ConcurrentHashMap<>();
        for (RepositoryPath paths : repositoryPathList) {
            String metaDataPath = paths.getTarget().toString();
            String name = paths.getFileName().toString();
            metaDataPath = metaDataPath.replace(name, String.format(".%s.foLibrary-metadata/metadata.json", name));
            if (Files.exists(Path.of(metaDataPath))) {
                JSONObject data = JSONObject.parseObject(Files.readString(Path.of(metaDataPath)));
                if (data != null && data.containsKey("RELEASE")) {
                    JSONObject release = data.getJSONObject("RELEASE");
                    releaseMap.put(release.getString("value"), paths);
                }
            }
        }
       if(!releaseMap.isEmpty()){
          String key =  releaseMap.keySet().stream().max(Comparator.comparing(ComparableVersion::new)).get();
          return releaseMap.get(key);
       }
       return null;
    }

}
