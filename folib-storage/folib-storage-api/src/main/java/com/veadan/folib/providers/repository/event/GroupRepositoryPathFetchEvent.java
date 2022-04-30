package com.veadan.folib.providers.repository.event;

import com.veadan.folib.event.RepositoryBasedEvent;
import com.veadan.folib.providers.io.RepositoryPath;

/**
 * @author Przemyslaw Fusik
 */
public class GroupRepositoryPathFetchEvent
        extends RepositoryBasedEvent<RepositoryPath>
{
    public GroupRepositoryPathFetchEvent(final RepositoryPath path)
    {
        super(path, -1);
    }
}
