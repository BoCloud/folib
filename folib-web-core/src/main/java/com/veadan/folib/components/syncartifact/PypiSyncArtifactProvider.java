package com.veadan.folib.components.syncartifact;

import com.veadan.folib.components.artifact.ArtifactComponent;
import com.veadan.folib.components.common.CommonComponent;
import com.veadan.folib.components.files.FilesCommonComponent;
import com.veadan.folib.configuration.ConfigurationManager;
import com.veadan.folib.enums.ArtifactSyncTypeEnum;
import com.veadan.folib.forms.syncartifact.SyncArtifactForm;
import com.veadan.folib.providers.io.RepositoryPathResolver;
import com.veadan.folib.services.ArtifactResolutionService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Lazy;
import org.springframework.stereotype.Component;

import javax.annotation.PostConstruct;
import javax.inject.Inject;

/**
 * @author leipenghui
 **/
@Slf4j
@Component
public class PypiSyncArtifactProvider implements SyncArtifactProvider {

    @Value("${folib.temp}")
    private String tempPath;

    @Inject
    private ConfigurationManager configurationManager;

    @Inject
    private SyncArtifactProviderRegistry syncArtifactProviderRegistry;

    @Inject
    private ArtifactComponent artifactComponent;

    @Inject
    protected ArtifactResolutionService artifactResolutionService;

    @Inject
    private RepositoryPathResolver repositoryPathResolver;

    @Inject
    private FilesCommonComponent filesCommonComponent;

    @Inject
    @Lazy
    private CommonComponent commonComponent;

    @PostConstruct
    @Override
    public void register() {
        syncArtifactProviderRegistry.addProvider(ArtifactSyncTypeEnum.PYPI.getType(), this);
        log.info("Registered sync artifact '{}' with alias '{}'.",
                getClass().getCanonicalName(), ArtifactSyncTypeEnum.PYPI.getType());
    }

    @Override
    public void browseFullSync(SyncArtifactForm syncArtifactForm) {

    }

    @Override
    public void fullSync(SyncArtifactForm syncArtifactForm) {

    }
}
