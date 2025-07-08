package com.folib.event.artifact;

import com.folib.event.Event;
import lombok.Getter;
import lombok.Setter;

@Setter
@Getter
public class PromoteDispenseEvent extends Event {
    private String sourceStorageId;
    private String sourceRepositoryId;
    private String sourcePath;
    private String targetStorageId;
    private String targetRepositoryId;
    private String targetUrl;
    private String syncNo;
    private int syncStatus;

    public PromoteDispenseEvent(String sourceStorageId,
                                String sourceRepositoryId,
                                String sourcePath,
                                String targetStorageId,
                                String targetRepositoryId,
                                String syncNo,
                                int syncStatus,
                                String targetUrl,
                                int type) {
        super(type);
        this.sourceStorageId = sourceStorageId;
        this.sourceRepositoryId = sourceRepositoryId;
        this.sourcePath = sourcePath;
        this.targetStorageId = targetStorageId;
        this.targetRepositoryId = targetRepositoryId;
        this.syncNo = syncNo;
        this.syncStatus = syncStatus;
        this.targetUrl = targetUrl;
    }

}
