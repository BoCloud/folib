package com.veadan.folib.services.impl;

import com.veadan.folib.constant.ArtifactSyncRecordStatusEnum;
import com.veadan.folib.entity.ArtifactSyncSlaveRecord;
import com.veadan.folib.mapper.ArtifactSyncRecordMapper;
import com.veadan.folib.mapper.ArtifactSyncSlaveRecordMapper;
import com.veadan.folib.model.request.ArtifactSyncSlaveRecordAddReq;
import com.veadan.folib.model.request.ArtifactSyncSlaveRecordUpdateReq;
import com.veadan.folib.services.ArtifactSyncSlaveRecordService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

/**
 * @author veadan

 * @date 2023/12/20 14:05
 */
@Service
public class ArtifactSyncSlaveRecordServiceImpl implements ArtifactSyncSlaveRecordService {
    @Autowired
    private ArtifactSyncSlaveRecordMapper artifactSyncSlaveRecordMapper;
    @Autowired
    private ArtifactSyncRecordMapper artifactSyncRecordMapper;

    @Override
    public Long add(ArtifactSyncSlaveRecordAddReq model) {
        final ArtifactSyncSlaveRecord newEntity = this.addModelToEntity(model);
        artifactSyncSlaveRecordMapper.insert(newEntity);
        return newEntity.getId();
    }

    @Override
    public Map<String, Long> batchAdd(List<ArtifactSyncSlaveRecordAddReq> models) {
        final List<ArtifactSyncSlaveRecord> newEntitiyList = models.stream().map(this::addModelToEntity).collect(Collectors.toList());
        artifactSyncSlaveRecordMapper.insert(newEntitiyList);
        return newEntitiyList.stream().collect(Collectors.toMap(ArtifactSyncSlaveRecord::getTempId, ArtifactSyncSlaveRecord::getId));
    }

    @Override
    public Boolean update(ArtifactSyncSlaveRecordUpdateReq model) {
        final Long id = model.getId();
        final Integer status = model.getStatus();
        final String failedReason = model.getFailedReason();
        final Date updateTime = model.getUpdateTime();
///        final String updateBy = model.getUpdateBy();
        boolean found = artifactSyncSlaveRecordMapper.updateRecordStatus(id, status, updateTime, failedReason);
        ArtifactSyncSlaveRecord record = artifactSyncSlaveRecordMapper.selectById(id);
        updateRecordStatus( status, record.getSyncNo(),failedReason);
        return found;

    }

    @Override
    public Boolean batchUpdate(List<ArtifactSyncSlaveRecordUpdateReq> models) {
        return models.stream().allMatch(this::update);
    }

    private ArtifactSyncSlaveRecord addModelToEntity(ArtifactSyncSlaveRecordAddReq model) {
        final ArtifactSyncSlaveRecord artifactSyncSlaveRecord = new ArtifactSyncSlaveRecord();
        artifactSyncSlaveRecord.setSourcePath(model.getSourcePath());
        artifactSyncSlaveRecord.setTargetPath(model.getTargetPath());
        artifactSyncSlaveRecord.setSyncNo(model.getSyncNo());
        artifactSyncSlaveRecord.setSyncModel(model.getSyncModel());
        artifactSyncSlaveRecord.setStatus(model.getStatus());
        artifactSyncSlaveRecord.setFailedReason(model.getFailedReason());
        artifactSyncSlaveRecord.setCreateBy(model.getCreateBy());
        artifactSyncSlaveRecord.setCreateTime(model.getCreateTime());
        artifactSyncSlaveRecord.setTempId(model.getTempId());
        return artifactSyncSlaveRecord;
    }

    public void updateRecordStatus(Integer status, String syncNo, String failedReason){
        if(ArtifactSyncRecordStatusEnum.SUCCESS.getVal().equals(status)){
            List<ArtifactSyncSlaveRecord> artifactSyncSlaveRecords =artifactSyncSlaveRecordMapper.selectBySyncNo(syncNo);
            long count = artifactSyncSlaveRecords.stream().filter(artifactSyncSlaveRecord -> ArtifactSyncRecordStatusEnum.SUCCESS.getVal().equals(artifactSyncSlaveRecord.getStatus())).count();
            if(count == artifactSyncSlaveRecords.size()){
                artifactSyncRecordMapper.updateStatusAndFailedReasonBySyncNo(status,"",syncNo,new Date());
            }
        }else if(ArtifactSyncRecordStatusEnum.FAILED.getVal().equals(status)){
            artifactSyncRecordMapper.updateStatusAndFailedReasonBySyncNo(status,failedReason,syncNo,new Date());
        }

    }
}
