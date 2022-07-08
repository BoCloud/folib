package com.veadan.folib.storage.repository.remote.heartbeat;

import com.veadan.folib.storage.repository.remote.RemoteRepository;

/**
 * @author xuxinping
 *
 */
public interface RemoteRepositoryAlivenessService
{

    boolean isAlive(RemoteRepository remoteRepository);

    void put(RemoteRepository remoteRepository,
             boolean aliveness);
    
}
