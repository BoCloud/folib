package com.veadan.folib.components.replication;

import com.veadan.folib.storage.repository.Repository;

/**
 * @author huayanjun
 * @since 2025-01-23 15:48
 */
public abstract class RemoteReplication {

    void fullReplication(Repository repository,String type) {

    }

    void browseFullReplication(Repository repository){
        // 下载

    }




    void incrementalReplication(Repository repository) {

    }
}
