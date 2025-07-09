package com.folib.providers.layout;

import cn.hutool.crypto.digest.SM3;
import com.folib.artifact.coordinates.DebianCoordinates;
import com.folib.constant.DebianConstant;
import com.folib.providers.io.RepositoryFileAttributeType;
import com.folib.providers.io.RepositoryFiles;
import com.folib.providers.io.RepositoryPath;
import com.folib.repository.DebianRepositoryFeatures;
import com.folib.repository.DebianRepositoryStrategy;
import com.folib.repository.RepositoryStrategy;
import com.folib.storage.repository.RepositoryTypeEnum;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.codec.digest.MessageDigestAlgorithms;
import org.apache.commons.lang3.BooleanUtils;
import org.springframework.stereotype.Component;

import javax.annotation.PostConstruct;
import javax.inject.Inject;
import java.io.IOException;
import java.time.Instant;
import java.time.temporal.ChronoUnit;
import java.util.Map;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/**
 * @author veadan
 * @since 2024-08-27 17:16
 */
@Slf4j
@Component
public class DebianLayoutProvider extends AbstractLayoutProvider<DebianCoordinates> {


    public static final String ALIAS = DebianConstant.LAYOUT_NAME;

    @Inject
    private DebianRepositoryStrategy debianRepositoryManagementStrategy;

    @Inject
    private DebianRepositoryFeatures debianRepositoryFeatures;


    @PostConstruct
    public void register() {
        log.info("Registered layout provider '{}' with alias '{}'.", getClass().getCanonicalName(), ALIAS);

    }

    // 解析路径 增加自定义解析路径 与jfrog 一致deb.distribution=123;deb.component=123;deb.architecture=123
    @Override
    public DebianCoordinates getArtifactCoordinates(RepositoryPath path)
            throws IOException {
        DebianCoordinates coordinates = DebianCoordinates.parse(RepositoryFiles.relativizePath(path));
        Map<String, String> extAttribute = path.getExtAttribute();
        if(extAttribute!=null){
            String distribution=extAttribute.get(DebianConstant.ATTR_DISTRIBUTION);
            String component=extAttribute.get(DebianConstant.ATTR_COMPONENT);
            String architecture=extAttribute.get(DebianConstant.ATTR_ARCHITECTURE);
            if(distribution!=null){
                coordinates.setDistribution(distribution);
            }
            if(component!=null){
                coordinates.setComponent(component);
            }
            if(distribution!=null){
                coordinates.setArchitecture(architecture);
            }
        }
        return coordinates;
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
                case REFRESH_CONTENT:
                    final Instant halfAnHourAgo = Instant.now().minus(refreshContentInterval(repositoryPath), ChronoUnit.MINUTES);
                    value = BooleanUtils.isTrue((Boolean) value) || (!RepositoryTypeEnum.HOSTED.getType().equals(repositoryPath.getRepository().getType()) && isPackage(repositoryPath))
                            &&
                            !RepositoryFiles.wasModifiedAfter(repositoryPath,
                                    halfAnHourAgo);
                    result.put(attributeType, value);
                default:
                    break;
            }
        }

        return result;
    }

    private boolean isPackage(RepositoryPath repositoryPath){
        String item=repositoryPath.getFileName().toString();
        return item.startsWith("Packages") || item.equals("Release") || item.equals("InRelease");
    }


    @Override
    public RepositoryStrategy getRepositoryManagementStrategy() {
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
        return Stream.of(MessageDigestAlgorithms.MD5, MessageDigestAlgorithms.SHA_1, MessageDigestAlgorithms.SHA_256, MessageDigestAlgorithms.SHA_512, SM3.ALGORITHM_NAME)
                .collect(Collectors.toSet());
    }

}
