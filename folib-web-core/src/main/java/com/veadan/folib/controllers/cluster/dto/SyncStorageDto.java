package com.veadan.folib.controllers.cluster.dto;

import com.veadan.folib.cluster.SyncStorageEnum;
import com.veadan.folib.storage.StorageDto;

public class SyncStorageDto {
    private StorageDto storageDto;

    private SyncStorageEnum syncStorageEnum;

    private String storageId;

    public SyncStorageDto() {
    }

    public SyncStorageDto(StorageDto storageDto,String storageId, SyncStorageEnum syncStorageEnum) {
        this.storageDto = storageDto;
        this.storageId = storageId;
        this.syncStorageEnum = syncStorageEnum;
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
}
