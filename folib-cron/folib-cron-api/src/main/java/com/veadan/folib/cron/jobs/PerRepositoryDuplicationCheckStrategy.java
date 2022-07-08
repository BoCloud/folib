package com.veadan.folib.cron.jobs;

import com.veadan.folib.cron.domain.CronTaskConfigurationDto;

import java.util.Collection;
import java.util.Objects;

import org.apache.commons.collections4.CollectionUtils;

/**
 * @author veadan
 */
public class PerRepositoryDuplicationCheckStrategy
        implements CronJobDuplicationCheckStrategy
{

    private static final PerRepositoryDuplicationCheckStrategy DEFAULT = new PerRepositoryDuplicationCheckStrategy();

    private final String propertyStorageId;

    private final String propertyRepositoryId;

    private PerRepositoryDuplicationCheckStrategy()
    {
        this("storageId", "repositoryId");
    }

    public PerRepositoryDuplicationCheckStrategy(final String propertyStorageId,
                                                 final String propertyRepositoryId)
    {
        this.propertyStorageId = propertyStorageId;
        this.propertyRepositoryId = propertyRepositoryId;
    }

    public static PerRepositoryDuplicationCheckStrategy getDefault()
    {
        return DEFAULT;
    }

    @Override
    public boolean duplicates(final CronTaskConfigurationDto candidate,
                              final Collection<CronTaskConfigurationDto> existing)
    {
        if (CollectionUtils.isEmpty(existing))
        {
            return false;
        }
        return existing.stream().filter(e -> duplicates(candidate, e)).findFirst().isPresent();
    }

    private boolean duplicates(final CronTaskConfigurationDto first,
                               final CronTaskConfigurationDto second)
    {
        if (first == null || second == null)
        {
            return false;
        }

        final String firstJobClass = first.getJobClass();
        final String secondJobClass = second.getJobClass();

        if (!Objects.equals(firstJobClass, secondJobClass))
        {
            return false;
        }

        if (Objects.equals(first.getUuid(), second.getUuid()))
        {
            return false;
        }

        final String firstStorageId = first.getProperty(propertyStorageId);
        final String firstRepositoryId = first.getProperty(propertyRepositoryId);

        final String secondStorageId = second.getProperty(propertyStorageId);
        final String secondRepositoryId = second.getProperty(propertyRepositoryId);

        return Objects.equals(firstStorageId, secondStorageId) &&
               Objects.equals(firstRepositoryId, secondRepositoryId);
    }
}
