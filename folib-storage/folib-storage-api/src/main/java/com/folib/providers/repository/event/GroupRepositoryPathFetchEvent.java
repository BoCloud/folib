package com.folib.providers.repository.event;

import com.folib.event.RepositoryBasedEvent;
import com.folib.providers.io.RepositoryPath;

/**
 * @author veadan
 */
public class GroupRepositoryPathFetchEvent
        extends RepositoryBasedEvent<RepositoryPath>
{
    public GroupRepositoryPathFetchEvent(final RepositoryPath path)
    {
        super(path, -1);
    }
}
