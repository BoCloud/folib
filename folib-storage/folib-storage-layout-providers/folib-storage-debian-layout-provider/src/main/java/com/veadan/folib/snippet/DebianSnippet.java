package com.veadan.folib.snippet;

import com.veadan.folib.artifact.coordinates.ArtifactCoordinates;
import com.veadan.folib.artifact.coordinates.DebianArtifactCoordinates;
import com.veadan.folib.constant.DebianConstant;
import com.veadan.folib.dependency.snippet.CompatibleDependencyFormatRegistry;
import com.veadan.folib.dependency.snippet.DependencySynonymFormatter;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

import javax.annotation.PostConstruct;
import javax.inject.Inject;

/**
 * @author huayanjun
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
