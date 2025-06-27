package com.veadan.folib.services;

import com.veadan.folib.domain.migrate.AddRepositoryForm;
import com.veadan.folib.domain.migrate.ArtifactMigrateInfo;
import com.veadan.folib.entity.Dict;
import com.veadan.folib.entity.MigrateInfo;
import com.veadan.folib.dto.JfrogMigrateDto;
import com.veadan.folib.scanner.common.msg.TableResultResponse;

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


    void migrate(JfrogMigrateDto form);

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
