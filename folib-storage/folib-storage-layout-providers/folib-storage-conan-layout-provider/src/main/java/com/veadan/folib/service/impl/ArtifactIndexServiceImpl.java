package com.veadan.folib.service.impl;

import com.alibaba.fastjson.JSONObject;
import com.google.common.collect.Lists;
import com.google.common.collect.Maps;
import com.veadan.folib.artifact.coordinates.ConanArtifactIndex;
import com.veadan.folib.configuration.Configuration;
import com.veadan.folib.configuration.ConfigurationManager;
import com.veadan.folib.domain.ConanPackagesRevisions;
import com.veadan.folib.domain.ConanRevision;
import com.veadan.folib.domain.ConanRevisions;
import com.veadan.folib.event.artifact.ArtifactEventListenerRegistry;
import com.veadan.folib.providers.io.RepositoryFiles;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.providers.io.RepositoryPathResolver;
import com.veadan.folib.providers.layout.ConanLayoutProvider;
import com.veadan.folib.service.ArtifactIndexService;
import com.veadan.folib.storage.Storage;
import com.veadan.folib.storage.repository.Repository;
import com.veadan.folib.util.CommonUtils;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.MapUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.springframework.stereotype.Component;

import javax.inject.Inject;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.attribute.BasicFileAttributes;
import java.util.Comparator;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.stream.Stream;

/**
 * @author leipenghui
 * @date 2024/3/20
 **/
@Slf4j
@Component
public class ArtifactIndexServiceImpl implements ArtifactIndexService {

    @Inject
    private ConfigurationManager configurationManager;

    @Inject
    private ArtifactEventListenerRegistry artifactEventListenerRegistry;

    @Inject
    private RepositoryPathResolver repositoryPathResolver;

    @Override
    public void rebuildIndex(String storageId, String repositoryId, String artifactPath) {
        Storage storage = getConfiguration().getStorage(storageId);
        Repository repository = storage.getRepository(repositoryId);

        if (!ConanLayoutProvider.ALIAS.equals(repository.getLayout())) {
            log.warn("Trying to rebuild index of repository {} with unsupported layout {} ", repository.getId(),
                    repository.getLayout());
            return;
        }

        RepositoryPath repositoryBasePath = repositoryPathResolver.resolve(repository);
        if (artifactPath != null && artifactPath.trim().length() > 0) {
            repositoryBasePath = repositoryBasePath.resolve(artifactPath);
        }
        if (!Files.exists(repositoryBasePath)) {
            return;
        }

        try (Stream<Path> pathStream = Files.walk(repositoryBasePath)) {
            pathStream.filter(Files::isDirectory)
                    // Skip directories which start with a dot (like, for example: .index)
                    .filter(ConanArtifactIndex::isIndexDirectory)
                    // Note: Sorting can be expensive:
                    .sorted()
                    .forEach(this::execute);
        } catch (IOException ex) {
            log.error(ExceptionUtils.getStackTrace(ex));
        }

    }

    @Override
    public void rebuildIndex(RepositoryPath repositoryPath) {
        try {
            rebuildIndex(repositoryPath.getStorageId(), repositoryPath.getRepositoryId(), RepositoryFiles.relativizePath(repositoryPath));
        } catch (Exception ex) {
            log.error(ExceptionUtils.getStackTrace(ex));
        }
    }

    private void execute(Path path) {
        try {
            RepositoryPath repositoryPath = (RepositoryPath) path;
            ConanArtifactIndex conanArtifactIndex = ConanArtifactIndex.parse(RepositoryFiles.relativizePath(repositoryPath));
            if (Objects.isNull(conanArtifactIndex)) {
                return;
            }
            String user = conanArtifactIndex.getUser(), name = conanArtifactIndex.getName(), version = conanArtifactIndex.getVersion(), channel = conanArtifactIndex.getChannel(), revisionId = conanArtifactIndex.getRevisionId(), packageId = conanArtifactIndex.getPackageId();
            Map<String, Long> map = Maps.newHashMap();
            try (Stream<Path> pathStream = Files.list(repositoryPath)) {
                pathStream.filter(ConanArtifactIndex::include).forEach(item -> {
                    try {
                        BasicFileAttributes attributes = Files.readAttributes(item, BasicFileAttributes.class);
                        map.put(item.getFileName().toString(), attributes.lastModifiedTime().toMillis());
                    } catch (Exception ex) {
                        log.error(ExceptionUtils.getStackTrace(ex));
                    }
                });
            }
            RepositoryPath indexJsonRepositoryPath = repositoryPath.resolve(ConanArtifactIndex.INDEX_JSON_NAME);
            if (MapUtils.isEmpty(map)) {
                Files.deleteIfExists(indexJsonRepositoryPath);
                return;
            }
            List<ConanRevision> revisions = Lists.newArrayList();
            map.entrySet().stream()
                    .sorted(Map.Entry.comparingByValue(Comparator.reverseOrder()))
                    .forEach(entry -> {
                        revisions.add(ConanRevision.builder().revision(entry.getKey()).time(CommonUtils.getConvertMillis2String(entry.getValue())).build());
                    });
            if (StringUtils.isBlank(revisionId)) {
                String reference = String.format("%s/%s@%s/%s", name, version, user, channel);
                ConanRevisions conanRevisions = ConanRevisions.builder().reference(reference).build();
                conanRevisions.setRevisions(revisions);
                Files.writeString(indexJsonRepositoryPath, JSONObject.toJSONString(conanRevisions));
            } else {
                String packageReference = String.format("%s/%s@%s/%s#%s:%s", name, version, user, channel, revisionId, packageId);
                ConanPackagesRevisions conanPackagesRevisions = ConanPackagesRevisions.builder().packageReference(packageReference).build();
                conanPackagesRevisions.setRevisions(revisions);
                Files.writeString(indexJsonRepositoryPath, JSONObject.toJSONString(conanPackagesRevisions));
            }
            if (StringUtils.isNotBlank(conanArtifactIndex.getRootIndexRelativizePath())) {
                execute(repositoryPathResolver.resolve(repositoryPath.getStorageId(), repositoryPath.getRepositoryId(), conanArtifactIndex.getRootIndexRelativizePath()));
            }
        } catch (Exception ex) {
            log.error("Rebuild index error [{}]", ExceptionUtils.getStackTrace(ex));
        }
    }

    public Configuration getConfiguration() {
        return configurationManager.getConfiguration();
    }
}
