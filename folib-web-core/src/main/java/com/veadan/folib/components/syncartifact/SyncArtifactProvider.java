package com.veadan.folib.components.syncartifact;

import com.veadan.folib.forms.syncartifact.SyncArtifactForm;

/**
 * @author leipenghui
 **/
public interface SyncArtifactProvider {

    /**
     * 注册
     */
    void register();

    /**
     * 全量同步
     *
     * @param syncArtifactForm 参数
     */
    void fullSync(SyncArtifactForm syncArtifactForm);
}
