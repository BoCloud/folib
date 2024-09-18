package com.veadan.folib.providers.layout;

import com.veadan.folib.artifact.coordinates.DebianArtifactCoordinates;
import com.veadan.folib.constant.DebianConstant;
import com.veadan.folib.providers.io.RepositoryFileAttributeType;
import com.veadan.folib.providers.io.RepositoryFiles;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.repository.DebianRepositoryFeatures;
import com.veadan.folib.repository.DebianRepositoryManagementStrategy;
import com.veadan.folib.repository.RepositoryManagementStrategy;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.codec.digest.MessageDigestAlgorithms;
import org.springframework.stereotype.Component;

import javax.annotation.PostConstruct;
import javax.inject.Inject;
import java.io.IOException;
import java.util.Map;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/**
 * @author huayanjun
 * @since 2024-08-27 17:16
 */
@Slf4j
@Component
public class DebianLayoutProvider extends AbstractLayoutProvider<DebianArtifactCoordinates> {


    public static final String ALIAS = DebianConstant.LAYOUT_NAME;

    @Inject
    private DebianRepositoryManagementStrategy debianRepositoryManagementStrategy;

    @Inject
    private DebianRepositoryFeatures debianRepositoryFeatures;


    @PostConstruct
    public void register() {
        log.info("Registered layout provider '{}' with alias '{}'.", getClass().getCanonicalName(), ALIAS);

    }

    // 解析路径
    @Override
    public DebianArtifactCoordinates getArtifactCoordinates(RepositoryPath path)
            throws IOException {
        return DebianArtifactCoordinates.parse(RepositoryFiles.relativizePath(path));
    }


    @Override
    public boolean isArtifactMetadata(RepositoryPath path) {
        return false;
    }

    public boolean isDebMetadata(RepositoryPath path) {
        String fileName = path.getFileName().toString();
        Matcher matcher = DebianConstant.META_PATTERN.matcher(fileName);
        return !matcher.matches();
    }


    @Override
    protected Map<RepositoryFileAttributeType, Object> getRepositoryFileAttributes(RepositoryPath repositoryPath,
                                                                                   RepositoryFileAttributeType... attributeTypes)
            throws IOException {
        Map<RepositoryFileAttributeType, Object> result = super.getRepositoryFileAttributes(repositoryPath,
                attributeTypes);

        for (RepositoryFileAttributeType attributeType : attributeTypes) {
            Object value = result.get(attributeType);
            switch (attributeType) {
                case ARTIFACT:
                    value = (Boolean) value && !isDebMetadata(repositoryPath);
                    if (value != null) {
                        result.put(attributeType, value);
                    }
                    break;
                case METADATA:
                    value = (Boolean) value || isDebMetadata(repositoryPath);
                    if (value != null) {
                        result.put(attributeType, value);
                    }
                    break;
                default:
                    break;
            }
        }

        return result;
    }


    @Override
    public RepositoryManagementStrategy getRepositoryManagementStrategy() {
        return debianRepositoryManagementStrategy;
    }

    @Override
    public Set<String> getDefaultArtifactCoordinateValidators() {
        return debianRepositoryFeatures.getDefaultArtifactCoordinateValidators();
    }

    @Override
    public String getAlias() {
        return ALIAS;
    }

    @Override
    public Set<String> getDigestAlgorithmSet() {
        return Stream.of(MessageDigestAlgorithms.MD5, MessageDigestAlgorithms.SHA_1, MessageDigestAlgorithms.SHA_256, MessageDigestAlgorithms.SHA_512)
                .collect(Collectors.toSet());
    }

}
