package com.folib.security.resolvepath;

import com.folib.artifact.coordinates.NpmCoordinates;
import com.folib.security.enums.ResolvePathTypeEnum;
import com.folib.storage.repository.Repository;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.io.FilenameUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.stereotype.Component;

import javax.annotation.PostConstruct;
import javax.inject.Inject;
import java.util.Objects;

/**
 * @author veadan
 **/
@Slf4j
@Component
public class NpmResolvePathProvider implements ResolvePathProvider {

    public static final String BINARY = "-/binary/";

    public static final String STORAGE = "/storages/";

    @Inject
    private ResolvePathProviderRegistry resolvePathProviderRegistry;

    @PostConstruct
    @Override
    public void register() {
        resolvePathProviderRegistry.addProvider(ResolvePathTypeEnum.NPM.getType(), this);
        log.info("Registered resolve path '{}' with alias '{}'.",
                getClass().getCanonicalName(), ResolvePathTypeEnum.NPM.getType());
    }

    @Override
    public String resolvePath(Repository repository, String relativePath) {
        if (StringUtils.isBlank(relativePath)) {
            return "";
        }
        String extension = FilenameUtils.getExtension(relativePath);
        if (!relativePath.startsWith(STORAGE) && !relativePath.startsWith(BINARY) && StringUtils.isNotBlank(extension) && NpmCoordinates.NPM_EXTENSION_PATTERN.matcher(extension).matches()) {
            NpmCoordinates npmArtifactCoordinates = NpmCoordinates.parseByResolvePath(relativePath);
            if (Objects.nonNull(npmArtifactCoordinates)) {
                relativePath = npmArtifactCoordinates.buildPath();
            }
        }
        return relativePath;
    }
}
