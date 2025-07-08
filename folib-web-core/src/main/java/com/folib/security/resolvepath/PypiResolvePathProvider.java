package com.folib.security.resolvepath;

import com.folib.artifact.coordinates.PypiArtifactCoordinates;
import com.folib.security.enums.ResolvePathTypeEnum;
import com.folib.storage.repository.Repository;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.stereotype.Component;

import javax.annotation.PostConstruct;
import javax.inject.Inject;

/**
 * @author veadan
 **/
@Slf4j
@Component
public class PypiResolvePathProvider implements ResolvePathProvider {

    public static final String SIMPLE = "simple/";

    public static final String PACKAGES = "packages/";

    @Inject
    private ResolvePathProviderRegistry resolvePathProviderRegistry;

    @PostConstruct
    @Override
    public void register() {
        resolvePathProviderRegistry.addProvider(ResolvePathTypeEnum.PYPI.getType(), this);
        log.info("Registered resolve path '{}' with alias '{}'.",
                getClass().getCanonicalName(), ResolvePathTypeEnum.PYPI.getType());
    }

    @Override
    public String resolvePath(Repository repository, String relativePath) {
        if (StringUtils.isBlank(relativePath)) {
            return "";
        }
        if (relativePath.startsWith(SIMPLE)) {
            relativePath = "";
        } else if (relativePath.startsWith(PACKAGES)) {
            relativePath = relativePath.replace("packages/", "");
            PypiArtifactCoordinates coordinates;
            try {
                coordinates = PypiArtifactCoordinates.parse(relativePath);
                relativePath = coordinates.buildPath();
            } catch (Exception e) {
                log.error("Invalid package name - {}", e.getMessage());
            }
        }
        return relativePath;
    }
}
