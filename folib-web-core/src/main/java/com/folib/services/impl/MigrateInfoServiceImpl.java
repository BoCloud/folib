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
package com.folib.services.impl;

import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.github.pagehelper.PageInfo;
import com.folib.domain.migrate.SyncArtifactForm;
import com.folib.entity.MigrateInfo;
import com.folib.mapper.MigrateInfoMapper;
import com.folib.services.MigrateInfoService;
import org.apache.commons.lang3.StringUtils;
import org.springframework.stereotype.Service;

import java.util.List;

/**
 * @author veadan
 * @since 2024-12-31 20:49
 */
@Service
public class MigrateInfoServiceImpl implements MigrateInfoService {

    private final MigrateInfoMapper migrateInfoMapper;

    public MigrateInfoServiceImpl(MigrateInfoMapper migrateInfoMapper) {
        this.migrateInfoMapper = migrateInfoMapper;
    }

    @Override
    public void save(MigrateInfo migrateInfo) {
        migrateInfoMapper.insert(migrateInfo);
    }

    @Override
    public void updateById(MigrateInfo migrateInfo) {
        migrateInfoMapper.updateById(migrateInfo);
    }

    @Override
    public PageInfo<MigrateInfo> selectByMigrateIdAndStatus(String migrateId, List<Integer> status, Integer pageNum, Integer pageSize,String repoName) {
        return PageInfo.of( migrateInfoMapper.selectList(Wrappers.<MigrateInfo>lambdaQuery()
                .eq(MigrateInfo::getMigrateId, migrateId)
                .in(MigrateInfo::getSyncStatus, status)
                .like(StringUtils.isNotEmpty(repoName), MigrateInfo::getRepositoryId, "%" + repoName + "%")
        ));
    }

    @Override
    public List<MigrateInfo> selectByMigrateId(String migrateId,  List<Integer> status) {
       return migrateInfoMapper.selectList(Wrappers.<MigrateInfo>lambdaQuery()
                .eq(MigrateInfo::getMigrateId, migrateId)
                .in(status != null && !status.isEmpty(), MigrateInfo::getSyncStatus, status)
        );

    }

    @Override
    public int countByMigrateId(String migrateId) {
       return Math.toIntExact(migrateInfoMapper.selectCount(Wrappers.<MigrateInfo>lambdaQuery().eq(MigrateInfo::getMigrateId, migrateId)));
    }

    @Override
    public MigrateInfo getByMigrateIdAndRepoInfo(String migrateId, String storageId, String repositoryId) {
        List<MigrateInfo> migrateInfos = migrateInfoMapper.selectList(Wrappers.<MigrateInfo>lambdaQuery()
                .eq(MigrateInfo::getMigrateId, migrateId)
                .eq(MigrateInfo::getStorageId, storageId)
                .eq(MigrateInfo::getRepositoryId, repositoryId)
        );
        if (migrateInfos.isEmpty()) {
            return null;
        } else {
            return migrateInfos.get(0);
        }
    }

    @Override
    public void updateAndSyncRepoStatus(SyncArtifactForm syncArtifactForm, int status) {
        MigrateInfo record = getByMigrateIdAndRepoInfo(syncArtifactForm.getMigrateId(), syncArtifactForm.getStorageId(), syncArtifactForm.getRepositoryId());
        record.setSyncStatus(status);
        record.setSuccessMount(syncArtifactForm.getSyncMount());
        updateById(record);

    }

    @Override
    public MigrateInfo getById(Long id) {
        return migrateInfoMapper.selectById(id);
    }

    @Override
    public void deleteByMigrateId(String migrateId) {
        migrateInfoMapper.delete(Wrappers.<MigrateInfo>lambdaQuery().eq(MigrateInfo::getMigrateId, migrateId));
    }


}
