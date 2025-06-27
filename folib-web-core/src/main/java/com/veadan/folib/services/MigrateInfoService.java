package com.veadan.folib.services;

import com.github.pagehelper.PageInfo;
import com.veadan.folib.domain.migrate.SyncArtifactForm;
import com.veadan.folib.entity.MigrateInfo;

import java.util.List;

/**
 * @author veadan
 * @since 2024-12-31 20:49
 */
public interface MigrateInfoService {

    void save(MigrateInfo migrateInfo);

    void updateById(MigrateInfo migrateInfo);

    PageInfo<MigrateInfo> selectByMigrateIdAndStatus(String migrateId, List<Integer> status, Integer pageNum, Integer pageSize, String repoName);

    List<MigrateInfo> selectByMigrateId(String migrateId, List<Integer> status);

    int countByMigrateId(String migrateId);

    MigrateInfo getByMigrateIdAndRepoInfo(String migrateId, String storageId, String repositoryId);

    void updateAndSyncRepoStatus(SyncArtifactForm syncArtifactForm, int status);

    MigrateInfo getById(Long id);

    void deleteByMigrateId(String migrateId);


}
