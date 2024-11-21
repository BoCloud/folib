package com.veadan.folib.components.syncartifact;

import com.veadan.folib.forms.syncartifact.SyncArtifactForm;
import com.veadan.folib.providers.layout.GoLayoutProvider;
import com.veadan.folib.providers.layout.Maven2LayoutProvider;
import com.veadan.folib.providers.layout.RawLayoutProvider;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

import javax.annotation.PostConstruct;
import javax.annotation.Resource;
import javax.inject.Inject;

/**
 * @author huayanjun
 * @since 2024-11-19 13:39
 */

@Slf4j
@Component
public class GoSyncArtifactProvider implements SyncArtifactProvider {

    @Resource
    private SyncArtifactProviderRegistry syncArtifactProviderRegistry;
    @Override
    @PostConstruct
    public void register() {
        syncArtifactProviderRegistry.addProvider(GoLayoutProvider.ALIAS, this);
        log.info("Registered sync artifact '{}' with alias '{}'.",
                getClass().getCanonicalName(), GoLayoutProvider.ALIAS);

    }

    @Override
    public void browseFullSync(SyncArtifactForm syncArtifactForm) {
        try{

        }catch (Exception e){

        }finally {

        }

    }

    @Override
    public void fullSync(SyncArtifactForm syncArtifactForm) {

    }
}
