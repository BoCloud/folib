package com.veadan.folib.controllers.cluster.dto;

import com.veadan.folib.cluster.SyncRepositoryEnum;
import com.veadan.folib.storage.repository.RepositoryDto;

public class SyncRepositoryDto {
    private RepositoryDto repositoryDto;

    private String storageId;

    private String repositoryId;

    private SyncRepositoryEnum syncRepositoryEnum;

    public SyncRepositoryDto() {
    }

    public SyncRepositoryDto(RepositoryDto repositoryDto, String storageId, String repositoryId, SyncRepositoryEnum syncRepositoryEnum) {
        this.repositoryDto = repositoryDto;
        this.storageId = storageId;
        this.repositoryId = repositoryId;
        this.syncRepositoryEnum = syncRepositoryEnum;
    }

    public RepositoryDto getRepositoryDto() {
        return repositoryDto;
    }

    public void setRepositoryDto(RepositoryDto repositoryDto) {
        this.repositoryDto = repositoryDto;
    }

    public String getStorageId() {
        return storageId;
    }

    public void setStorageId(String storageId) {
        this.storageId = storageId;
    }

    public SyncRepositoryEnum getSycnRepositoryEnum() {
        return syncRepositoryEnum;
    }

    public void setSycnRepositoryEnum(SyncRepositoryEnum syncRepositoryEnum) {
        this.syncRepositoryEnum = syncRepositoryEnum;
    }

    public String getRepositoryId() {
        return repositoryId;
    }

    public void setRepositoryId(String repositoryId) {
        this.repositoryId = repositoryId;
    }
}
