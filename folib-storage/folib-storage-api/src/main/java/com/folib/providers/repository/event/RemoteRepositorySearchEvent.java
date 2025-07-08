package com.folib.providers.repository.event;

import com.folib.data.criteria.Paginator;
import com.folib.event.Event;
import com.folib.providers.repository.RepositorySearchRequest;

public class RemoteRepositorySearchEvent extends Event
{

    private String storageId;
    private String repositoryId;
    private RepositorySearchRequest predicate;
    private Paginator paginator = new Paginator();

    public RemoteRepositorySearchEvent(String sorageId,
                                       String repositoryId,
                                       RepositorySearchRequest predicate,
                                       Paginator paginator)
    {
        super(-1);
        this.storageId = sorageId;
        this.repositoryId = repositoryId;
        this.predicate = predicate;
        this.paginator = paginator;
    }

    public String getStorageId()
    {
        return storageId;
    }

    public void setStorageId(String storageId)
    {
        this.storageId = storageId;
    }

    public String getRepositoryId()
    {
        return repositoryId;
    }

    public void setRepositoryId(String repositoryId)
    {
        this.repositoryId = repositoryId;
    }

    public RepositorySearchRequest getPredicate()
    {
        return predicate;
    }

    public void setPredicate(RepositorySearchRequest predicate)
    {
        this.predicate = predicate;
    }

    public Paginator getPaginator()
    {
        return paginator;
    }

    public void setPaginator(Paginator paginator)
    {
        this.paginator = paginator;
    }

}
