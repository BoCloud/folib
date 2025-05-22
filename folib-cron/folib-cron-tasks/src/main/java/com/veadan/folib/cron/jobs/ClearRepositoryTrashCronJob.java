package com.veadan.folib.cron.jobs;

import com.google.common.collect.Maps;
import com.veadan.folib.cron.jobs.fields.*;
import com.veadan.folib.services.RepositoryManagementService;
import com.veadan.folib.cron.domain.CronTaskConfigurationDto;
import com.veadan.folib.cron.jobs.fields.*;

import jakarta.inject.Inject;
import java.util.Map;
import java.util.Set;

import com.google.common.collect.ImmutableSet;
import org.apache.commons.lang3.StringUtils;

/**
 * @author veadan
 */
public class ClearRepositoryTrashCronJob
        extends JavaCronJob
{

    private static final String PROPERTY_STORAGE_ID = "storageId";

    private static final String PROPERTY_REPOSITORY_ID = "repositoryId";

    private static final String PROPERTY_STORAGE_DAY = "storageDay";

    private static final Set<CronJobField> FIELDS = ImmutableSet.of(
            new CronJobStorageIdAutocompleteField(new CronJobStringTypeField(
                    new CronJobOptionalField(new CronJobNamedField(PROPERTY_STORAGE_ID)))),
            new CronJobRepositoryIdAutocompleteField(new CronJobStringTypeField(
                    new CronJobOptionalField(new CronJobNamedField(PROPERTY_REPOSITORY_ID)))),
            new CronJobIntegerTypeField(
                    new CronJobOptionalField(new CronJobAliasNamedField(new CronJobNamedField(PROPERTY_STORAGE_DAY), "保留天数"))));

    @Inject
    private RepositoryManagementService repositoryManagementService;

    @Override
    public void executeTask(CronTaskConfigurationDto config)
            throws Throwable
    {
        String storageId = config.getProperty(PROPERTY_STORAGE_ID);
        String repositoryId = config.getProperty(PROPERTY_REPOSITORY_ID);
        String storageDay = config.getProperty(PROPERTY_STORAGE_DAY);
        Map<String, String> properties = config.getProperties();
        String artifactPathPrefix = "artifactPath:";
        Map<String, String> cleanupArtifactPathMap = Maps.newLinkedHashMap();
        for (Map.Entry<String, String> entry : properties.entrySet()) {
            String key = entry.getKey();
            if (!key.startsWith(artifactPathPrefix)) {
                continue;
            }
            key = key.replace(artifactPathPrefix, "");
            if (StringUtils.isBlank(key) || StringUtils.isBlank(entry.getValue())) {
                continue;
            }
            cleanupArtifactPathMap.put(key, entry.getValue());
        }
        if (storageId == null && repositoryId == null)
        {
            repositoryManagementService.deleteTrash(true,  storageDay, cleanupArtifactPathMap);
        }
        else
        {
            repositoryManagementService.deleteTrash(storageId, repositoryId, storageDay, cleanupArtifactPathMap);
        }
    }

    @Override
    public CronJobDefinition getCronJobDefinition()
    {
        return CronJobDefinition.newBuilder()
                                .jobClass(ClearRepositoryTrashCronJob.class.getName())
                                .name("仓库垃圾回收站定时清理任务")
                                .scope(GLOBAL)
                                .description("该任务可定时删除制品仓库下的.trash垃圾回收站下的文件")
                                .fields(FIELDS)
                                .build();
    }

}
