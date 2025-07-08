package com.folib.repository;

import com.folib.job.cron.domain.CronTaskConfigurationDto;
import com.folib.job.cron.services.CronTaskDataService;
import com.folib.providers.io.RepositoryPath;
import com.folib.providers.io.RepositoryPathResolver;
import com.folib.job.tasks.DownloadRemoteMavenIndexCronJob;
import com.folib.job.tasks.MergeMavenGroupRepositoryIndexCronJob;
import com.folib.job.tasks.RebuildMavenIndexesCronJob;
import com.folib.storage.repository.Repository;

import javax.inject.Inject;
import java.io.IOException;
import java.nio.file.Files;

import org.springframework.context.annotation.Lazy;
import org.springframework.stereotype.Component;

/**
 * @author Veadan
 */
@Component
public class MavenRepositoryManagementStrategy
        extends AbstractRepositoryManagementStrategy
{
    @Lazy
    @Inject
    private CronTaskDataService cronTaskDataService;
    @Lazy
    @Inject
    private MavenRepositoryFeatures repositoryFeatures;

    @Lazy
    @Inject
    private RepositoryPathResolver repositoryPathResolver;

//    @Override
//    protected void createRepositoryInternal(Storage storage,
//                                            Repository repository)
//            throws RepositoryManagementStrategyException
//    {
//        if (!repositoryFeatures.isIndexingEnabled(repository))
//        {
//            return;
//        }
//
//        String storageId = storage.getId();
//        String repositoryId = repository.getId();
//        MavenRepositoryConfiguration repositoryConfig =
//                (MavenRepositoryConfiguration) repository.getRepositoryConfiguration();
//
//        if (repository.isHostedRepository())
//        {
//            createRebuildMavenIndexCronJob(storageId, repositoryId, repositoryConfig.getCronExpression());
//        }
//        if (repository.isProxyRepository())
//        {
//            createRemoteIndexDownloaderCronTask(storageId, repositoryId, repositoryConfig.getCronExpression());
//        }
//        if (repository.isGroupRepository())
//        {
//            createMergeMavenGroupRepositoryIndexCronJob(storageId, repositoryId, repositoryConfig.getCronExpression());
//        }
//    }

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
