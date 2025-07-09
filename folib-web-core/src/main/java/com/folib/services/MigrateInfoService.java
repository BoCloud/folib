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

import com.github.pagehelper.PageInfo;
import com.folib.domain.migrate.SyncArtifactForm;
import com.folib.entity.MigrateInfo;

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
