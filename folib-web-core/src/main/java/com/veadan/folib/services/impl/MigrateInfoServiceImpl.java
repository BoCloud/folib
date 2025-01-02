package com.veadan.folib.services.impl;

import com.github.pagehelper.PageHelper;
import com.github.pagehelper.PageInfo;
import com.veadan.folib.entity.AccessToken;
import com.veadan.folib.entity.MigrateInfo;
import com.veadan.folib.forms.syncartifact.SyncArtifactForm;
import com.veadan.folib.mapper.MigrateInfoMapper;
import com.veadan.folib.services.MigrateInfoService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.util.StringUtils;
import tk.mybatis.mapper.entity.Example;

import java.util.List;

/**
 * @author huayanjun
 * @since 2024-12-31 20:49
 */
@Service
public class MigrateInfoServiceImpl implements MigrateInfoService {

    private final MigrateInfoMapper migrateInfoMapper;

    public MigrateInfoServiceImpl(MigrateInfoMapper migrateInfoMapper){
        this.migrateInfoMapper=migrateInfoMapper;
    }
    @Override
    public void save(MigrateInfo migrateInfo) {
        migrateInfoMapper.insert(migrateInfo);
    }

    @Override
    public void updateById(MigrateInfo migrateInfo) {
        migrateInfoMapper.updateByPrimaryKey(migrateInfo);
    }

    @Override
    public PageInfo<MigrateInfo> selectByMigrateIdAndStatus(String migrateId, List<Integer> status, Integer pageNum, Integer pageSize) {
        PageHelper.startPage(pageNum, pageSize);
        Example example = Example.builder(MigrateInfo.class).build();
        Example.Criteria where = example.createCriteria();
        where.andEqualTo("migrateId",migrateId);
        where.andIn("syncStatus",status);
        return PageInfo.of(migrateInfoMapper.selectByExample(example));
    }

    @Override
    public int countByMigrateId(String migrateId) {
        Example example = Example.builder(MigrateInfo.class).build();
        Example.Criteria where = example.createCriteria();
        where.andEqualTo("migrateId",migrateId);
        return migrateInfoMapper.selectCountByExample(example);
    }

    @Override
    public MigrateInfo getByMigrateIdAndRepoInfo(String migrateId, String storageId, String repositoryId) {
        Example example = Example.builder(MigrateInfo.class).build();
        Example.Criteria where = example.createCriteria();
        where.andEqualTo("migrateId",migrateId);
        where.andEqualTo("storageId",storageId);
        where.andEqualTo("repositoryId",repositoryId);
        List<MigrateInfo> migrateInfos = migrateInfoMapper.selectByExample(example);
        if(migrateInfos.size()<1){
            return null;
        }else {
            return migrateInfos.get(0);
        }
    }

    @Override
    public void updateAndSyncRepoStatus(SyncArtifactForm syncArtifactForm, int status) {
        MigrateInfo record = getByMigrateIdAndRepoInfo(syncArtifactForm.getMigrateId(), syncArtifactForm.getStorageId(), syncArtifactForm.getRepositoryId());
        record.setSyncStatus(status);
        updateById(record);

    }


}
