/*
 * Folib - [新一代AI制品仓库]
 * Copyright (C) 2025 bocloud.com.cn <folib@beyondcent.com>
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * 本程序是自由软件：您可依据GNU通用公共许可证（GPL-3.0+）条款重新发布和修改，
 * 但禁止任何形式的商业售卖行为（包括但不限于：直接销售、捆绑销售、云服务商用）。
 *
 * This program is distributed WITHOUT ANY WARRANTY.
 * Commercial sale of this software is expressly prohibited.
 *
 * For license details, see: https://www.gnu.org/licenses/gpl-3.0.html
 * 商业授权咨询请联系：folib@beyondcent.com
 */
package com.folib.job.tasks;

import com.folib.job.cron.jobs.fields.*;
import com.google.common.collect.ImmutableSet;
import com.folib.configuration.ConfigurationManager;
import com.folib.job.cron.domain.CronTaskConfigurationDto;
import com.folib.job.cron.jobs.CronJobDefinition;
import com.folib.job.cron.jobs.JavaCronJob;
import com.folib.providers.layout.Maven2LayoutProvider;
import com.folib.repository.MavenRepositoryFeatures;
import com.folib.storage.Storage;
import com.folib.storage.repository.Repository;
import com.folib.storage.repository.RepositoryPolicyEnum;
import org.codehaus.plexus.util.xml.pull.XmlPullParserException;

import jakarta.inject.Inject;
import java.io.IOException;
import java.security.NoSuchAlgorithmException;
import java.util.Map;
import java.util.Set;

/**
 * @author veadan
 */
public class RemoveTimestampedMavenSnapshotCronJob
        extends JavaCronJob {

    private static final String PROPERTY_STORAGE_ID = "storageId";

    private static final String PROPERTY_REPOSITORY_ID = "repositoryId";

    private static final String PROPERTY_BASE_PATH = "basePath";

    private static final String PROPERTY_NUMBER_TO_KEEP = "numberToKeep";

    private static final String PROPERTY_KEEP_PERIOD = "keepPeriod";

    private static final Set<CronJobField> FIELDS = ImmutableSet.of(
            new CronJobStorageIdAutocompleteField(new CronJobStringTypeField(
                    new CronJobOptionalField(new CronJobNamedField(PROPERTY_STORAGE_ID)))),
            new CronJobRepositoryIdAutocompleteField(new CronJobStringTypeField(
                    new CronJobOptionalField(new CronJobNamedField(PROPERTY_REPOSITORY_ID)))),
            new CronJobStringTypeField(
                    new CronJobOptionalField(new CronJobAliasNamedField(new CronJobNamedField(PROPERTY_BASE_PATH), "基础路径"))),
            new CronJobIntegerTypeField(
                    new CronJobOptionalField(new CronJobAliasNamedField(new CronJobNamedField(PROPERTY_NUMBER_TO_KEEP), "保留个数"))),
            new CronJobIntegerTypeField(
                    new CronJobOptionalField(new CronJobAliasNamedField(new CronJobNamedField(PROPERTY_KEEP_PERIOD), "保留天数"))));

    @Inject
    private MavenRepositoryFeatures mavenRepositoryFeatures;

    @Inject
    private ConfigurationManager configurationManager;

    @Override
    public void executeTask(CronTaskConfigurationDto config)
            throws Throwable {
        String storageId = config.getProperty(PROPERTY_STORAGE_ID);
        String repositoryId = config.getProperty(PROPERTY_REPOSITORY_ID);
        String basePath = config.getProperty(PROPERTY_BASE_PATH);

        // The number of artifacts to keep
        int numberToKeep = config.getProperty(PROPERTY_NUMBER_TO_KEEP) != null ?
                Integer.parseInt(config.getProperty(PROPERTY_NUMBER_TO_KEEP)) :
                0;

        // The period to keep artifacts (the number of days)
        int keepPeriod = config.getProperty(PROPERTY_KEEP_PERIOD) != null ?
                Integer.parseInt(config.getProperty(PROPERTY_KEEP_PERIOD)) :
                0;

        if (storageId == null) {
            Map<String, Storage> storages = getStorages();
            for (String storage : storages.keySet()) {
                removeTimestampedSnapshotArtifacts(storage, numberToKeep, keepPeriod);
            }
        } else if (repositoryId == null) {
            removeTimestampedSnapshotArtifacts(storageId, numberToKeep, keepPeriod);
        } else {
            mavenRepositoryFeatures.removeTimestampedSnapshots(storageId,
                    repositoryId,
                    basePath,
                    numberToKeep,
                    keepPeriod);
        }
    }

    @Override
    public CronJobDefinition getCronJobDefinition() {
        return CronJobDefinition.newBuilder()
                .jobClass(RemoveTimestampedMavenSnapshotCronJob.class.getName())
                .name("定时删除SNAPSHOT的Maven制品").scope(MAVEN)
                .description("定时删除SNAPSHOT，带有时间日期快照的制品包")
                .fields(FIELDS)
                .build();
    }

    /**
     * To remove timestamped snapshot artifacts in repositories
     *
     * @param storageId    path of storage
     * @param numberToKeep the number of artifacts to keep
     * @param keepPeriod   the period to keep artifacts (the number of days)
     * @throws NoSuchAlgorithmException
     * @throws XmlPullParserException
     * @throws IOException
     */
    private void removeTimestampedSnapshotArtifacts(String storageId,
                                                    int numberToKeep,
                                                    int keepPeriod)
            throws NoSuchAlgorithmException,
            XmlPullParserException,
            IOException {
        Map<String, ? extends Repository> repositories = getRepositories(storageId);

        repositories.forEach((repositoryId, repository) ->
        {
            if (Maven2LayoutProvider.ALIAS.equals(repository.getLayout()) && repository.getPolicy().equals(RepositoryPolicyEnum.SNAPSHOT.getPolicy()) || repository.getPolicy().equals(RepositoryPolicyEnum.MIXED.getPolicy())) {
                try {
                    mavenRepositoryFeatures.removeTimestampedSnapshots(storageId,
                            repositoryId,
                            null,
                            numberToKeep,
                            keepPeriod);
                } catch (IOException e) {
                    logger.error(e.getMessage(), e);
                }
            }
        });
    }

    private Map<String, Storage> getStorages() {
        return configurationManager.getConfiguration().getStorages();
    }

    private Map<String, ? extends Repository> getRepositories(String storageId) {
        return getStorages().get(storageId).getRepositories();
    }

}
