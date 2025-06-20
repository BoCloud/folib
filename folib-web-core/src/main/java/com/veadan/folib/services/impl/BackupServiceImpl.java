package com.veadan.folib.services.impl;

import com.google.common.collect.Lists;
import com.veadan.folib.entity.Dict;
import com.veadan.folib.enums.DictTypeEnum;
import com.veadan.folib.dto.backup.BackupDto;
import com.veadan.folib.dto.common.RepositoryDto;
import com.veadan.folib.services.BackupService;
import com.veadan.folib.services.DictService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;
import java.util.Objects;

/**
 * @author leipenghui
 * @date 2023/9/26
 **/
@Slf4j
@Service
public class BackupServiceImpl implements BackupService {

    @Autowired
    private DictService dictService;

    @Transactional(rollbackFor = Exception.class)
    @Override
    public void saveBackup(BackupDto backupForm) {
        String dictKey = "", dictValue = backupForm.getDirectoryPath();
        List<Dict> dictList = Lists.newArrayList();
        Dict dict = null, dbDict = null;
        for (RepositoryDto repositoryForm : backupForm.getRepositoryList()) {
            dictKey = String.format("%s:%s", repositoryForm.getStorageId(), repositoryForm.getRepositoryId());
            dict = Dict.builder().dictType(DictTypeEnum.BACKUP_SETTINGS.getType()).dictKey(dictKey).dictValue(dictValue).build();
            dbDict = dictService.selectLatestOneDict(dict);
            if (Objects.nonNull(dbDict)) {
                dictService.deleteDictById(dbDict.getId());
            }
            dictList.add(dict);
        }
        dictService.batchInsertDict(dictList);
    }
}
