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
package com.folib.services;

import com.folib.domain.migrate.AddRepositoryForm;
import com.folib.domain.migrate.ArtifactMigrateInfo;
import com.folib.entity.Dict;
import com.folib.entity.MigrateInfo;
import com.folib.forms.JfrogMigrateForm;
import com.folib.scanner.common.msg.TableResultResponse;

import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Queue;

/**
 * @author veadan
 * @since 2024-10-22 17:01
 */
public interface JfrogMigrateService {

    String TOPIC_QUEUE = "artifact_migrate_topic_queue";

    String TOPIC_PAUSED = "artifact_migrate_topic_paused";

    // 0-暂停 1-继续
    String PAUSED_FLAG_PRE = "migrate:pause:";

    String ARTIFACT_COUNT = "migrate:artifact:count:";
    String INDEX_COUNT = "migrate:index:count:";

    String DIRECTORY_TOTAl = "migrate:directory:total:";
    String DIRECTORY_COUNT = "migrate:directory:count:";


    Queue<String> PAUSED_QUEUE = new LinkedList<>();


    void migrate(JfrogMigrateForm form);

    List<Dict> getMigrateTask();

    TableResultResponse<MigrateInfo> getRepositoryByMigrateId(int page, int limit, String migrateId, String status,String repoName);

    void addSyncRepository(AddRepositoryForm form);

    void startMigrate(String migrateId, List<String> storeAndRepo);

    void pauseMigrate(String migrateId, List<String> storeAndRepo);

    void setFailed(List<Long> ids);

    void addTask(ArtifactMigrateInfo info);

    void updateTask(Long id, ArtifactMigrateInfo info);

    void listenTask(String migrateId);

    Map<String, String> getFinishedCount(String migrateId, List<String> storeAndRepos);

    Map<String, Long> getIndexCount(String migrateId, List<String> storeAndRepos);

    void repoContinue(String migrateId, List<String> storeAndRepos);

    void repoFinish(String migrateId, List<String> storeAndRepos);

    void changeLayout(MigrateInfo info);

    void deleteTask(Long id);

    void restartRepo(String migrateId);

    List<String> getAllRepo(String migrateId);

     Dict getWebhookSetting();



}
