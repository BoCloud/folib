package com.veadan.folib.services.impl;

import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.github.pagehelper.PageHelper;
import com.github.pagehelper.PageInfo;
import com.veadan.folib.domain.migrate.SyncArtifactForm;
import com.veadan.folib.entity.MigrateInfo;
import com.veadan.folib.mapper.MigrateInfoMapper;
import com.veadan.folib.services.MigrateInfoService;
import org.apache.commons.lang3.StringUtils;
import org.springframework.stereotype.Service;

import java.util.List;

/**
 * @author huayanjun
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
