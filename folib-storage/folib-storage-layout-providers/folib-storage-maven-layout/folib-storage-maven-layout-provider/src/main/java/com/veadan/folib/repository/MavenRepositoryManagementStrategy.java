package com.veadan.folib.repository;

import com.veadan.folib.cron.domain.CronTaskConfigurationDto;
import com.veadan.folib.cron.services.CronTaskDataService;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.providers.io.RepositoryPathResolver;
import com.veadan.folib.cron.jobs.DownloadRemoteMavenIndexCronJob;
import com.veadan.folib.cron.jobs.MergeMavenGroupRepositoryIndexCronJob;
import com.veadan.folib.cron.jobs.RebuildMavenIndexesCronJob;
import com.veadan.folib.storage.Storage;
import com.veadan.folib.storage.repository.Repository;
import com.veadan.folib.yaml.configuration.repository.MavenRepositoryConfiguration;

import javax.inject.Inject;
import java.io.IOException;
import java.nio.file.Files;

import org.springframework.stereotype.Component;

/**
 * @author carlspring
 */
@Component
public class MavenRepositoryManagementStrategy
        extends AbstractRepositoryManagementStrategy
{

    @Inject
    private CronTaskDataService cronTaskDataService;

    @Inject
    private MavenRepositoryFeatures repositoryFeatures;

    @Inject
    private RepositoryPathResolver repositoryPathResolver;

    @Override
    protected void createRepositoryInternal(Storage storage,
                                            Repository repository)
            throws RepositoryManagementStrategyException
    {
        if (!repositoryFeatures.isIndexingEnabled(repository))
        {
            return;
        }

        String storageId = storage.getId();
        String repositoryId = repository.getId();
        MavenRepositoryConfiguration repositoryConfig =
                (MavenRepositoryConfiguration) repository.getRepositoryConfiguration();

        if (repository.isHostedRepository())
        {
            createRebuildMavenIndexCronJob(storageId, repositoryId, repositoryConfig.getCronExpression());
        }
        if (repository.isProxyRepository())
        {
            createRemoteIndexDownloaderCronTask(storageId, repositoryId, repositoryConfig.getCronExpression());
        }
        if (repository.isGroupRepository())
        {
            createMergeMavenGroupRepositoryIndexCronJob(storageId, repositoryId, repositoryConfig.getCronExpression());
        }
    }

    private void createRemoteIndexDownloaderCronTask(String storageId,
                                                     String repositoryId,
                                                     String cronExpression)
            throws RepositoryManagementStrategyException
    {
        CronTaskConfigurationDto configuration = new CronTaskConfigurationDto();
        configuration.setName("Remote index download for " + storageId + ":" + repositoryId);
        configuration.setJobClass(DownloadRemoteMavenIndexCronJob.class.getName());
        configuration.setCronExpression(cronExpression);
        configuration.addProperty("storageId", storageId);
        configuration.addProperty("repositoryId", repositoryId);
        configuration.setImmediateExecution(true);

        try
        {
            cronTaskDataService.save(configuration);
        }
        catch (Exception e)
        {
            throw new RepositoryManagementStrategyException(e.getMessage(), e);
        }
    }

    private void createRebuildMavenIndexCronJob(String storageId,
                                                String repositoryId,
                                                String cronExpression)
            throws RepositoryManagementStrategyException
    {
        CronTaskConfigurationDto configuration = new CronTaskConfigurationDto();
        configuration.setName("Rebuild Maven Index Cron Job for " + storageId + ":" + repositoryId);
        configuration.setJobClass(RebuildMavenIndexesCronJob.class.getName());
        configuration.setCronExpression(cronExpression);
        configuration.addProperty("storageId", storageId);
        configuration.addProperty("repositoryId", repositoryId);
        configuration.setImmediateExecution(true);

        try
        {
            cronTaskDataService.save(configuration);
        }
        catch (Exception e)
        {
            throw new RepositoryManagementStrategyException(e.getMessage(), e);
        }
    }

    private void createMergeMavenGroupRepositoryIndexCronJob(String storageId,
                                                             String repositoryId,
                                                             String cronExpression)
            throws RepositoryManagementStrategyException
    {
        CronTaskConfigurationDto configuration = new CronTaskConfigurationDto();
        configuration.setName("Merge maven group repository index cron job " + storageId + ":" + repositoryId);
        configuration.setJobClass(MergeMavenGroupRepositoryIndexCronJob.class.getName());
        configuration.setCronExpression(cronExpression);
        configuration.addProperty("storageId", storageId);
        configuration.addProperty("repositoryId", repositoryId);
        configuration.setImmediateExecution(false);

        try
        {
            cronTaskDataService.save(configuration);
        }
        catch (Exception e)
        {
            throw new RepositoryManagementStrategyException(e.getMessage(), e);
        }
    }

    @Override
    public void createRepositoryStructure(final Repository repository)
            throws IOException
    {
        super.createRepositoryStructure(repository);

        final RepositoryPath rootRepositoryPath = repositoryPathResolver.resolve(repository);
        final RepositoryPath indexRepositoryPath = rootRepositoryPath.resolve(MavenRepositoryFeatures.INDEX);
        if (!Files.exists(indexRepositoryPath))
        {
            Files.createDirectories(indexRepositoryPath);
        }
    }

}
