package com.veadan.folib.event.index;

import com.veadan.folib.event.AbstractEventListenerRegistry;
import org.springframework.stereotype.Component;

@Component
public class IndexEventListenerRegistry extends AbstractEventListenerRegistry {

    public void dispatchUpdateIndexEvent(String storageId,
                                           String repositoryId,
                                           IndexTypeEnum type) {
        IndexEvent event = new IndexEvent(storageId,
                repositoryId,
                type);
        dispatchEvent(event);
    }
}
