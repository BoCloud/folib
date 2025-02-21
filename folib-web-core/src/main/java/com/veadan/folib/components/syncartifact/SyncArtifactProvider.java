package com.veadan.folib.components.syncartifact;


import com.veadan.folib.domain.migrate.SyncArtifactForm;

/**
 * @author leipenghui
 **/
public interface SyncArtifactProvider {
    /**
     * 注册
     */
    void register();

    /**
     * 全量同步-基于browse
     *
     * @param syncArtifactForm 参数
     */
    void browseFullSync(SyncArtifactForm syncArtifactForm);

    /**
     * 全量同步
     *
     * @param syncArtifactForm 参数
     */
    void fullSync(SyncArtifactForm syncArtifactForm);

    void batchBrowseSync(SyncArtifactForm syncArtifactForm);
}
