package com.folib.event.index;

import com.folib.event.Event;
import lombok.Getter;
import lombok.Setter;

@Setter
@Getter
public class IndexEvent extends Event {

    private String storageId;
    private String repositoryId;
    private IndexTypeEnum indexType;

    public IndexEvent(String storageId, String repositoryId,IndexTypeEnum type) {
        super(type);
        this.storageId = storageId;
        this.repositoryId = repositoryId;
        this.indexType = type;
    }

}
