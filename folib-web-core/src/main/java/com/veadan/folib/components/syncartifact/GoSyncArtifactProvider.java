package com.veadan.folib.components.syncartifact;

import com.veadan.folib.enums.ArtifactSyncTypeEnum;
import com.veadan.folib.forms.syncartifact.SyncArtifactForm;
import com.veadan.folib.services.MigrateInfoService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

import javax.annotation.PostConstruct;
import javax.annotation.Resource;
import javax.inject.Inject;

/**
 * @author huayanjun
 * @since 2024-12-19 15:54
 */

@Slf4j
@Component
public class GoSyncArtifactProvider implements SyncArtifactProvider{

    @Inject
    private SyncArtifactProviderRegistry syncArtifactProviderRegistry;
    @Resource
    private MigrateInfoService migrateInfoService;

    @Override
    @PostConstruct
    public void register() {
        syncArtifactProviderRegistry.addProvider(ArtifactSyncTypeEnum.GO.getType(), this);
        log.info("Registered sync artifact '{}' with alias '{}'.",
                getClass().getCanonicalName(), ArtifactSyncTypeEnum.GO.getType());


    }

    @Override
    public void browseFullSync(SyncArtifactForm syncArtifactForm) {

    }

    @Override
    public void fullSync(SyncArtifactForm syncArtifactForm) {

    }

    @Override
    public void batchBrowseSync(SyncArtifactForm syncArtifactForm) {

    }
}
