package com.veadan.folib.event;

import com.veadan.folib.event.artifact.ArtifactEvent;
import com.veadan.folib.index.indexer.CondaMetadataIndexer;
import com.veadan.folib.providers.io.RepositoryPath;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import javax.inject.Inject;

/**
 * @author LingengMa
 * @date 2025/04/21 10:19
 * @Description:
 */

@Component
public class CondaArtifactEventListener {
    private final CondaMetadataIndexer condaMetadataIndexer;

    @Autowired
    public CondaArtifactEventListener(CondaMetadataIndexer condaMetadataIndexer) {
        this.condaMetadataIndexer = condaMetadataIndexer;
    }

    @AsyncEventListener
    public void handle(final ArtifactEvent<RepositoryPath> event) {

    }
}
