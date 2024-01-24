package com.veadan.folib.providers.layout;


import cn.hutool.core.io.FileUtil;
import com.veadan.folib.artifact.coordinates.GoArtifactCoordinates;
import com.veadan.folib.providers.io.RepositoryFileAttributeType;
import com.veadan.folib.providers.io.RepositoryFiles;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.repository.GoRepositoryFeatures;
import com.veadan.folib.repository.GoRepositoryManagementStrategy;
import org.apache.commons.lang3.BooleanUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

import javax.annotation.PostConstruct;
import javax.inject.Inject;
import java.io.IOException;
import java.time.Instant;
import java.time.temporal.ChronoUnit;
import java.util.*;

/**
 * @author pengYongQiang
 * @date 1/3/2024 15:31
 */
@Component("goLayoutProvider")
public class GoLayoutProvider extends AbstractLayoutProvider<GoArtifactCoordinates> {

    private static final Logger logger = LoggerFactory.getLogger(GoLayoutProvider.class);

    public static final String ALIAS = GoArtifactCoordinates.LAYOUT_NAME;

    public static final Collection<String> GO_METADATA_SET;

    static {
        HashSet<String> set = new HashSet<>();
        set.add("list");
        set.add("@latest");
        set.add(".info");
        GO_METADATA_SET = Collections.unmodifiableSet(set);
    }

    @Inject
    private GoRepositoryManagementStrategy goRepositoryManagementStrategy;

    @Inject
    private GoRepositoryFeatures goRepositoryFeatures;


    @PostConstruct
    public void register() {
        logger.info("Registered layout provider '{}' with alias '{}'.", getClass().getCanonicalName(), ALIAS);
    }

    @Override
    protected GoArtifactCoordinates getArtifactCoordinates(RepositoryPath path) throws IOException {

        return GoArtifactCoordinates.parse(RepositoryFiles.relativizePath(path));
    }

    @Override
    public boolean isArtifactMetadata(RepositoryPath path) {
        return GO_METADATA_SET.stream().anyMatch(path::endsWith);
    }

    public boolean isArtifact(RepositoryPath path) {
        return "zip".equals(FileUtil.extName(path.getFileName().toString()));
    }


    @Override
    public GoRepositoryManagementStrategy getRepositoryManagementStrategy() {
        return goRepositoryManagementStrategy;
    }

    @Override
    public Set<String> getDefaultArtifactCoordinateValidators() {
        return goRepositoryFeatures.getDefaultArtifactCoordinateValidators();
    }

    @Override
    public String getAlias() {
        return ALIAS;
    }

    @Override
    protected Map<RepositoryFileAttributeType, Object> getRepositoryFileAttributes(RepositoryPath repositoryPath, RepositoryFileAttributeType... attributeTypes) throws IOException {
        Map<RepositoryFileAttributeType, Object> result = super.getRepositoryFileAttributes(repositoryPath,
                attributeTypes);

        for (RepositoryFileAttributeType attributeType : attributeTypes) {
            Object value = result.get(attributeType);
            switch (attributeType) {
                case ARTIFACT:
                    value = (Boolean) value && !isArtifactMetadata(repositoryPath) && isArtifact(repositoryPath);

                    result.put(attributeType, value);

                    break;
                case EXPIRED:
                    final Instant tenSecondsAgo = Instant.now().minus(60, ChronoUnit.SECONDS);
                    value = BooleanUtils.isTrue((Boolean) value) || (isArtifactMetadata(repositoryPath)
                            &&
                            !RepositoryFiles.wasModifiedAfter(repositoryPath,
                                    tenSecondsAgo));

                    result.put(attributeType, value);

                    break;
                default:

                    break;
            }
        }

        return result;
    }

}
