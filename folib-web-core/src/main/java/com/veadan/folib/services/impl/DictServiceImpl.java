package com.veadan.folib.services.impl;

import com.google.common.collect.Lists;
import com.veadan.folib.domain.CacheSettings;
import com.veadan.folib.entity.Dict;
import com.veadan.folib.enums.DictTypeEnum;
import com.veadan.folib.enums.UpgradeTaskStatusEnum;
import com.veadan.folib.forms.dict.DictForm;
import com.veadan.folib.mapper.DictMapper;
import com.veadan.folib.services.DictService;
import com.veadan.folib.util.CacheUtil;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import tk.mybatis.mapper.entity.Example;

import java.util.Date;
import java.util.List;
import java.util.Objects;

/**
 * @author leipenghui
 * @date 2023/2/28
 **/
@Slf4j
@Service
public class DictServiceImpl implements DictService {

    @Autowired
    private DictMapper dictMapper;

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void saveDict(Dict dict) {
        if (StringUtils.isBlank(dict.getDictType()) || StringUtils.isBlank(dict.getDictKey())) {
            return;
        }
        dict.setCreateTime(new Date());
        dict.setComment(handlerComment(dict));
        dictMapper.insertSelective(dict);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void updateDict(DictForm dictForm) {
        Dict dict = Dict.builder().build();
        BeanUtils.copyProperties(dictForm, dict);
        dict.setComment(handlerComment(dict));
        Dict dbDict = selectLatestOneDict(dict);
        if (Objects.isNull(dbDict)) {
            saveDict(dict);
        } else {
            Example example = Example.builder(Dict.class).build();
            Example.Criteria criteria = example.createCriteria();
            if (Objects.nonNull(dict.getId())) {
                criteria.andEqualTo("id", dict.getId());
            }
            if (StringUtils.isNotBlank(dict.getDictKey())) {
                criteria.andEqualTo("dictKey", dict.getDictKey());
            }
            if (StringUtils.isNotBlank(dict.getDictType())) {
                criteria.andEqualTo("dictType", dict.getDictType());
            }
            dictMapper.updateByExampleSelective(dict, example);
        }
        if (Boolean.TRUE.equals(dictForm.getOverrideSystemProperty())) {
            System.setProperty(dict.getDictKey(), dict.getDictValue());
            log.info("更新系统属性：key {}，value：{}", dict.getDictKey(), dict.getDictValue());
        }
        String key = DictTypeEnum.CACHE_SETTINGS.getType();
        if (key.equals(dict.getDictType())) {
            CacheUtil<String, CacheSettings> cacheUtil = CacheUtil.getInstance();
            cacheUtil.remove(key);
            CacheUtil<String, String> cachePathUtil = CacheUtil.getInstance();
            cachePathUtil.remove("ARTIFACT_CACHE_ROOT_PATH");
        }
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void saveOrUpdateDict(Dict dict, Boolean isOverride) {
        dict.setComment(handlerComment(dict));
        Dict dbDict = selectOneDict(dict);
        if (Objects.nonNull(dbDict)) {
            //dictValue值相同
            boolean flag = Boolean.FALSE.equals(isOverride) && StringUtils.isNotBlank(dict.getDictValue()) && dict.getDictValue().equals(dbDict.getDictValue());
            if (flag) {
                return;
            }
            DictForm dictForm = DictForm.builder().build();
            BeanUtils.copyProperties(dict, dictForm);
            updateDict(dictForm);
        } else {
            saveDict(dict);
        }
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void deleteDict(Dict dict) {
        Example example = Example.builder(Dict.class).build();
        Example.Criteria criteria = example.createCriteria();
        criteria.andEqualTo("dictType", dict.getDictType());
        if (StringUtils.isNotBlank(dict.getDictKey())) {
            criteria.andEqualTo("dictKey", dict.getDictKey());
        }
        int count = dictMapper.selectCountByExample(example);
        if (count > 0) {
            dictMapper.deleteByExample(example);
        }
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void deleteDictById(Long id) {
        dictMapper.deleteByPrimaryKey(id);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public List<Dict> selectDict(Dict dict) {
        deleteHistoryDataForUploadProcessBySeconds(null);
        Example example = Example.builder(Dict.class).build();
        Example.Criteria criteria = example.createCriteria();
        criteria.andEqualTo("dictType", dict.getDictType());
        if (StringUtils.isNotBlank(dict.getDictKey())) {
            criteria.andEqualTo("dictKey", dict.getDictKey());
        }
        example.setOrderByClause("create_time desc");
        return dictMapper.selectByExample(example);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public Dict selectOneDict(Dict dict) {
        deleteHistoryDataForUploadProcessBySeconds(null);
        return dictMapper.selectOneDict(dict);
    }

    @Override
    public Dict selectLatestOneDict(Dict dict) {
        if (Objects.nonNull(dict.getId())) {
            return dictMapper.selectByPrimaryKey(dict.getId());
        }
        Example example = Example.builder(Dict.class).build();
        Example.Criteria criteria = example.createCriteria();
        criteria.andEqualTo("dictType", dict.getDictType());
        if (StringUtils.isNotBlank(dict.getDictKey())) {
            criteria.andEqualTo("dictKey", dict.getDictKey());
        }
        if (StringUtils.isNotBlank(dict.getComment())) {
            criteria.andEqualTo("comment", dict.getComment());
        }
        example.setOrderByClause("create_time desc");
        List<Dict> dictList = dictMapper.selectByExample(example);
        dict = null;
        if (CollectionUtils.isNotEmpty(dictList)) {
            dict = dictList.get(0);
        }
        return dict;
    }

    @Override
    public List<Dict> selectLatestListDict(Dict dict) {
        Example example = Example.builder(Dict.class).build();
        Example.Criteria criteria = example.createCriteria();
        criteria.andEqualTo("dictType", dict.getDictType());
        if (StringUtils.isNotBlank(dict.getDictKey())) {
            criteria.andEqualTo("dictKey", dict.getDictKey());
        }
        if (StringUtils.isNotBlank(dict.getComment())) {
            criteria.andEqualTo("comment", dict.getComment());
        }
        example.setOrderByClause("create_time desc");
        return dictMapper.selectByExample(example);
    }

    @Override
    public List<Dict> selectUnExecutedTask() {
        Example example = Example.builder(Dict.class).build();
        Example.Criteria criteria = example.createCriteria();
        criteria.andEqualTo("dictType", DictTypeEnum.FOLIB_UPGRADE_TASK.getType());
        criteria.andIn("comment", Lists.newArrayList(UpgradeTaskStatusEnum.UN_EXECUTED.getStatus(), UpgradeTaskStatusEnum.EXECUTED_FAIL.getStatus()));
        example.setOrderByClause("create_time asc");
        return dictMapper.selectByExample(example);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void updateUnExecutedTask(Dict dict) {
        Example example = Example.builder(Dict.class).build();
        Example.Criteria criteria = example.createCriteria();
        criteria.andEqualTo("dictType", dict.getDictType());
        criteria.andEqualTo("dictKey", dict.getDictKey());
        criteria.andEqualTo("dictValue", dict.getDictValue());
        dictMapper.updateByExampleSelective(Dict.builder().comment(dict.getComment()).build(), example);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void batchInsertDict(List<Dict> dictList) {
        if (CollectionUtils.isEmpty(dictList)) {
            return;
        }
        List<List<Dict>> lists = Lists.partition(dictList, 100);
        for (List<Dict> list : lists) {
            dictMapper.batchInsertDict(list);
        }
    }

    private String handlerComment(Dict dict) {
        String comment = dict.getComment();
        int size = 255;
        if (StringUtils.isNotBlank(comment) && comment.length() > size) {
            comment = comment.substring(0, size - 1);
        }
        return comment;
    }

    /**
     * 删除上传进度的历史数据
     *
     * @param seconds 秒数
     */
    private void deleteHistoryDataForUploadProcessBySeconds(Integer seconds) {
        if (Objects.isNull(seconds)) {
            seconds = 28800;
        }
        dictMapper.deleteHistoryDataForUploadProcessBySeconds(seconds);
    }
}
