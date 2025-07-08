package com.folib.components.syncartifact;

import com.folib.constant.DebianConstant;
import com.folib.constant.GlobalConstants;
import com.folib.enums.ArtifactSyncTypeEnum;
import com.folib.services.MigrateInfoService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import javax.annotation.PostConstruct;
import javax.annotation.Resource;

/**
 * @author veadan
 * @since 2025-02-21 08:54
 */
@Slf4j
@Component
public class NewDebianSyncAtifactProvider  extends BaseArtifactProvider{


    @Resource
    private SyncArtifactProviderRegistry syncArtifactProviderRegistry;


    @Autowired
    public NewDebianSyncAtifactProvider(SyncUtils syncUtils, MigrateInfoService migrateInfoService) {
        super(syncUtils, migrateInfoService);
    }
    @Override
    @PostConstruct
    public void register() {
        syncArtifactProviderRegistry.addProvider(ArtifactSyncTypeEnum.DEBIAN.getType(), this);
        log.info("Registered sync artifact '{}' with alias '{}'.",
                getClass().getCanonicalName(), ArtifactSyncTypeEnum.DEBIAN.getType());
    }

    @Override
    public String getLayout() {
         return DebianConstant.LAYOUT_ALIAS;
    }

    @Override
    public boolean isArtifact(String url) {
        return !url.endsWith(GlobalConstants.SEPARATOR);
    }
}
