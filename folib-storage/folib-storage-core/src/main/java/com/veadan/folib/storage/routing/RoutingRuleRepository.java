package com.veadan.folib.storage.routing;

import javax.annotation.concurrent.Immutable;
import java.util.StringJoiner;

import org.apache.commons.lang3.StringUtils;

/**
 * @author Przemyslaw Fusik
 */
@Immutable
public class RoutingRuleRepository
        implements RepositoryIdentifiable
{

    private final String storageId;

    private final String repositoryId;

    public RoutingRuleRepository(MutableRoutingRuleRepository source)
    {
        this.storageId = source.getStorageId();
        this.repositoryId = source.getRepositoryId();
    }

    public String getStorageId()
    {
        return storageId;
    }

    public String getRepositoryId()
    {
        return repositoryId;
    }

    public String getStorageIdAndRepositoryId()
    {
        StringJoiner stringJoiner = new StringJoiner(":");

        if (StringUtils.isNotBlank(storageId))
        {
            stringJoiner.add(storageId);
        }

        if (StringUtils.isNotBlank(repositoryId))
        {
            stringJoiner.add(repositoryId);
        }

        return stringJoiner.toString();
    }
}
