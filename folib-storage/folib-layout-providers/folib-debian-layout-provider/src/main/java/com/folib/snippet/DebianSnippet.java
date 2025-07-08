package com.folib.snippet;

import com.folib.artifact.coordinates.ArtifactCoordinates;
import com.folib.artifact.coordinates.DebianArtifactCoordinates;
import com.folib.constant.DebianConstant;
import com.folib.dependency.snippet.CompatibleDependencyFormatRegistry;
import com.folib.dependency.snippet.DependencySynonymFormatter;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

import javax.annotation.PostConstruct;
import javax.inject.Inject;

/**
 * @author veadan
 */
@Slf4j
@Component
public class DebianSnippet
        implements DependencySynonymFormatter {

    @Inject
    private CompatibleDependencyFormatRegistry compatibleDependencyFormatRegistry;


    @PostConstruct
    @Override
    public void register() {
        compatibleDependencyFormatRegistry.addProviderImplementation(getLayout(), getFormatAlias(), this);
        log.info("Initialized the debian dependency formatter.");
    }

    @Override
    public String getLayout() {
        return DebianConstant.LAYOUT_ALIAS;
    }

    @Override
    public String getFormatAlias() {
        return DebianConstant.LAYOUT_ALIAS;
    }

    @Override
    public String getDependencySnippet(ArtifactCoordinates artifactCoordinates) {
        DebianArtifactCoordinates coordinates = (DebianArtifactCoordinates) artifactCoordinates;
        return String.format("apt install %s=%s", coordinates.getFileName(), coordinates.getVersion());
    }

}
