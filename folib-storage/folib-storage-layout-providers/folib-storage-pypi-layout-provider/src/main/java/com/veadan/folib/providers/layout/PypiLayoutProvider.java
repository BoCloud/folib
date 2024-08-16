package com.veadan.folib.providers.layout;

import com.veadan.folib.artifact.coordinates.PypiArtifactCoordinates;
import com.veadan.folib.constant.GlobalConstants;
import com.veadan.folib.constants.PypiConstants;
import com.veadan.folib.providers.io.RepositoryFileAttributeType;
import com.veadan.folib.providers.io.RepositoryFiles;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.repository.PypiRepositoryFeatures;
import com.veadan.folib.repository.PypiRepositoryManagementStrategy;
import com.veadan.folib.repository.RepositoryManagementStrategy;
import org.apache.commons.lang3.BooleanUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

import javax.annotation.PostConstruct;
import javax.inject.Inject;
import java.io.IOException;
import java.nio.file.Files;
import java.time.Instant;
import java.time.temporal.ChronoUnit;
import java.util.Map;
import java.util.Set;

/**
 * @author Veadan
 */
@Component
public class PypiLayoutProvider
        extends AbstractLayoutProvider<PypiArtifactCoordinates> {
    private static final Logger logger = LoggerFactory.getLogger(PypiLayoutProvider.class);

    public static final String ALIAS = PypiArtifactCoordinates.LAYOUT_NAME;

    @Inject
    private PypiRepositoryManagementStrategy pypiRepositoryManagementStrategy;

    @Inject
    private PypiRepositoryFeatures pypiRepositoryFeatures;


    @PostConstruct
    public void register() {
        logger.info("Registered layout provider '{}' with alias '{}'.",
                getClass().getCanonicalName(), ALIAS);
    }

    @Override
    public PypiArtifactCoordinates getArtifactCoordinates(RepositoryPath path) throws IOException {
        return PypiArtifactCoordinates.parse(RepositoryFiles.relativizePath(path));
    }

    @Override
    public boolean isArtifactMetadata(RepositoryPath path) {
        // TODO: Fix
        return false;
    }

    public boolean isMetadata(RepositoryPath path) {
        // TODO: Fix
        return false;
    }

    public boolean isPypiPackageHtml(RepositoryPath path) {
        return path.getFileName().toString().endsWith(".html");
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
                    value = (Boolean) value && !isMetadata(repositoryPath);

                    if (value != null) {
                        result.put(attributeType, value);
                    }

                    break;
                case METADATA:
                    value = (Boolean) value || isMetadata(repositoryPath);

                    if (value != null) {
                        result.put(attributeType, value);
                    }

                    break;
                case REFRESH_CONTENT:
                    final Instant halfAnHourAgo = Instant.now().minus(refreshContentInterval(repositoryPath), ChronoUnit.MINUTES);
                    value = BooleanUtils.isTrue((Boolean) value) || (Files.exists(repositoryPath) && isPypiPackageHtml(repositoryPath)
                            &&
                            !RepositoryFiles.wasModifiedAfter(repositoryPath,
                                    halfAnHourAgo));
                    result.put(attributeType, value);
                    break;
                default:

                    break;
            }
        }

        return result;
    }

    @Override
    public RepositoryManagementStrategy getRepositoryManagementStrategy() {
        return pypiRepositoryManagementStrategy;
    }

    @Override
    public Set<String> getDefaultArtifactCoordinateValidators() {
        return pypiRepositoryFeatures.getDefaultArtifactCoordinateValidators();
    }

    @Override
    public String getAlias() {
        return ALIAS;
    }

    @Override
    public void targetUrl(RepositoryPath repositoryPath) {
        String remoteUrl = repositoryPath.getRepository().getRemoteRepository().getUrl();
        remoteUrl = StringUtils.removeEnd(remoteUrl, GlobalConstants.SEPARATOR);
        if (remoteUrl.endsWith(PypiConstants.PYPI_SIMPLE)) {
            try {
                String artifactPath = RepositoryFiles.relativizePath(repositoryPath), imagePath = "";
                PypiArtifactCoordinates pypiArtifactCoordinates = PypiArtifactCoordinates.parse(artifactPath);
                String path = pypiArtifactCoordinates.getPath();
                remoteUrl = remoteUrl.replace(PypiConstants.PYPI_SIMPLE, PypiConstants.PYPI_PACKAGES);
                repositoryPath.setTargetUrl(path);
            } catch (Exception ex) {
                logger.warn("RepositoryPath [{}] parse coordinates error [{}]", repositoryPath, ExceptionUtils.getStackTrace(ex));
                throw new RuntimeException(ex.getMessage());
            }
        }
        repositoryPath.setTargetUrl(String.format("%s/%s", remoteUrl, StringUtils.removeStart(repositoryPath.getTargetUrl(), GlobalConstants.SEPARATOR)));
    }

}
