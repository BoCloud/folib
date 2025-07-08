package com.folib.providers.repository.event;

import com.folib.event.RepositoryBasedEvent;
import com.folib.providers.io.RepositoryPath;

/**
 * @author veadan
 */
public class ProxyRepositoryPathExpiredEvent
        extends RepositoryBasedEvent<RepositoryPath>
{
    public ProxyRepositoryPathExpiredEvent(final RepositoryPath path)
    {
        super(path, -1);
    }
}
