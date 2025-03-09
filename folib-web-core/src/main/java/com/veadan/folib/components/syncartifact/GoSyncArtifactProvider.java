package com.veadan.folib.components.syncartifact;

import com.veadan.folib.artifact.coordinates.GoArtifactCoordinates;
import com.veadan.folib.constant.GlobalConstants;
import com.veadan.folib.enums.ArtifactSyncTypeEnum;
import com.veadan.folib.services.MigrateInfoService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import javax.annotation.PostConstruct;
import javax.annotation.Resource;

/**
 * @author huayanjun
 * @since 2024-12-19 15:54
 */

@Slf4j
@Component
public class GoSyncArtifactProvider extends BaseArtifactProvider {

    @Resource
    private SyncArtifactProviderRegistry syncArtifactProviderRegistry;

    @Autowired
    public GoSyncArtifactProvider(SyncUtils syncUtils, MigrateInfoService migrateInfoService) {
        super(syncUtils, migrateInfoService);
    }

    @Override
    @PostConstruct
    public void register() {
        syncArtifactProviderRegistry.addProvider(ArtifactSyncTypeEnum.GO.getType(), this);
        log.info("Registered sync artifact '{}' with alias '{}'.",
                getClass().getCanonicalName(), ArtifactSyncTypeEnum.GO.getType());

    }

    @Override
    public String getLayout() {
        return GoArtifactCoordinates.LAYOUT_NAME;
    }


    @Override
    public boolean isArtifact(String url) {
        return !url.endsWith(GlobalConstants.SEPARATOR);
    }
}
