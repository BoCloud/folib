package com.veadan.folib.controllers.cluster.dto;

import com.veadan.folib.cluster.SyncStorageEnum;
import com.veadan.folib.storage.StorageDto;

public class SyncStorageDto {
    private StorageDto storageDto;

    private SyncStorageEnum syncStorageEnum;

    private String storageId;

    private Boolean deleteForceFlag;

    public SyncStorageDto() {
    }

    public SyncStorageDto(StorageDto storageDto,String storageId, SyncStorageEnum syncStorageEnum) {
        this.storageDto = storageDto;
        this.storageId = storageId;
        this.syncStorageEnum = syncStorageEnum;
    }

    public SyncStorageDto(StorageDto storageDto, SyncStorageEnum syncStorageEnum, String storageId, Boolean deleteForceFlag) {
        this.storageDto = storageDto;
        this.syncStorageEnum = syncStorageEnum;
        this.storageId = storageId;
        this.deleteForceFlag = deleteForceFlag;
    }

    public StorageDto getStorageDto() {
        return storageDto;
    }

    public void setStorageDto(StorageDto storageDto) {
        this.storageDto = storageDto;
    }

    public SyncStorageEnum getSycnStorageEnum() {
        return syncStorageEnum;
    }

    public void setSycnStorageEnum(SyncStorageEnum syncStorageEnum) {
        this.syncStorageEnum = syncStorageEnum;
    }

    public String getStorageId() {
        return storageId;
    }

    public void setStorageId(String storageId) {
        this.storageId = storageId;
    }

    public SyncStorageEnum getSyncStorageEnum() {
        return syncStorageEnum;
    }

    public void setSyncStorageEnum(SyncStorageEnum syncStorageEnum) {
        this.syncStorageEnum = syncStorageEnum;
    }

    public Boolean getDeleteForceFlag() {
        return deleteForceFlag;
    }

    public void setDeleteForceFlag(Boolean deleteForceFlag) {
        this.deleteForceFlag = deleteForceFlag;
    }
}
