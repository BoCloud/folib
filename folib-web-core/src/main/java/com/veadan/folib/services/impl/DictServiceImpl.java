package com.veadan.folib.services.impl;

import com.google.common.collect.Lists;
import com.veadan.folib.entity.Dict;
import com.veadan.folib.enums.UpgradeTaskStatusEnum;
import com.veadan.folib.mapper.DictMapper;
import com.veadan.folib.services.DictService;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
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
        dict.setCreateTime(new Date());
        dict.setComment(handlerComment(dict));
        dictMapper.insertSelective(dict);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void updateDict(Dict dict) {
        dict.setComment(handlerComment(dict));
        Example example = Example.builder(Dict.class).build();
        Example.Criteria criteria = example.createCriteria();
        criteria.andEqualTo("dictKey", dict.getDictKey());
        dictMapper.updateByExampleSelective(dict, example);
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
            updateDict(dict);
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
    public List<Dict> selectUnExecutedTask() {
        Example example = Example.builder(Dict.class).build();
        Example.Criteria criteria = example.createCriteria();
        criteria.andEqualTo("dictType", "folib_upgrade_task");
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
