package com.veadan.folib.services;

import com.veadan.folib.model.request.ArtifactSyncSlaveRecordAddReq;
import com.veadan.folib.model.request.ArtifactSyncSlaveRecordUpdateReq;

import java.util.List;
import java.util.Map;

/**
 * @author veadan
 * @date 2023/12/20 14:04
 */
public interface ArtifactSyncSlaveRecordService {
    
    Long add(ArtifactSyncSlaveRecordAddReq model);

    Map<String, Long> batchAdd(List<ArtifactSyncSlaveRecordAddReq> models);
    Boolean update(ArtifactSyncSlaveRecordUpdateReq model);

    Boolean batchUpdate(List<ArtifactSyncSlaveRecordUpdateReq> models);

}
