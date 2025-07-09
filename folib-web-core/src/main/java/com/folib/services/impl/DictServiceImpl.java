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

import com.github.pagehelper.PageHelper;
import com.github.pagehelper.PageInfo;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.google.common.collect.Lists;
import com.folib.components.DistributedCacheComponent;
import com.folib.constant.GlobalConstants;
import com.folib.domain.CacheSettings;
import com.folib.entity.Dict;
import com.folib.enums.DictTypeEnum;
import com.folib.enums.UpgradeTaskStatusEnum;
import com.folib.event.bucket.BucketEventListenerRegistry;
import com.folib.forms.dict.DictForm;
import com.folib.mapper.DictMapper;
import com.folib.scanner.common.msg.TableResultResponse;
import com.folib.services.DictService;
import com.folib.util.CacheUtil;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Lazy;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.Date;
import java.util.List;
import java.util.Objects;

/**
 * @author veadan
 * @date 2023/2/28
 **/
@Slf4j
@Service
public class DictServiceImpl implements DictService {

    @Autowired
    private DictMapper dictMapper;

    @Autowired
    @Lazy
    private DistributedCacheComponent distributedCacheComponent;

    @Autowired
    @Lazy
    private BucketEventListenerRegistry bucketEventListenerRegistry;

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void saveDict(Dict dict) {
        if (StringUtils.isBlank(dict.getDictType()) || StringUtils.isBlank(dict.getDictKey())) {
            return;
        }
        dict.setCreateTime(new Date());
        dict.setComment(handlerComment(dict));
        dictMapper.insert(dict);
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
            if (Objects.nonNull(dict.getId())) {
                dictMapper.update(dict, Wrappers.<Dict>lambdaUpdate()
                        .eq(Objects.nonNull(dict.getId()), Dict::getId, dict.getId())
                );
            }else {
                dictMapper.update(dict, Wrappers.<Dict>lambdaUpdate()
                        .eq(Objects.nonNull(dict.getDictKey()), Dict::getDictKey, dict.getDictKey())
                        .eq(Objects.nonNull(dict.getDictType()), Dict::getDictType, dict.getDictType())
                );
            }
        }
        if (Boolean.TRUE.equals(dictForm.getOverrideSystemProperty())) {
            System.setProperty(dict.getDictKey(), dict.getDictValue());
            log.info("更新系统属性 key [{}] value [{}]", dict.getDictKey(), dict.getDictValue());
            distributedCacheComponent.put(dict.getDictKey(), dict.getDictValue());
            if(GlobalConstants.BUCKET_CAPACITY_KEY.equals(dict.getDictKey())){
                bucketEventListenerRegistry.dispatchUpdateBucketEvent(Long.parseLong(dict.getDictValue()),0);
            }if(GlobalConstants.BUCKET_TOKENS_KEY.equals(dict.getDictKey())){
                bucketEventListenerRegistry.dispatchUpdateBucketEvent(0,Long.parseLong(dict.getDictValue()));
            }
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
        int count = Math.toIntExact(dictMapper.selectCount(Wrappers.<Dict>lambdaQuery()
                .eq(Dict::getDictType, dict.getDictType())
                .eq(StringUtils.isNotBlank(dict.getDictKey()), Dict::getDictKey, dict.getDictKey())
        ));
        if (count > 0) {
            dictMapper.delete(Wrappers.<Dict>lambdaQuery()
                    .eq(Dict::getDictType, dict.getDictType())
                    .eq(StringUtils.isNotBlank(dict.getDictKey()), Dict::getDictKey, dict.getDictKey())
            );
        }
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void deleteDictById(Long id) {
        dictMapper.deleteById(id);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public List<Dict> selectDict(Dict dict) {
        deleteHistoryDataForUploadProcessBySeconds(null);
       return dictMapper.selectList(Wrappers.<Dict>lambdaQuery()
                .eq(Dict::getDictType, dict.getDictType())
                .eq(StringUtils.isNotBlank(dict.getDictKey()), Dict::getDictKey, dict.getDictKey())
                .eq(StringUtils.isNotBlank(dict.getComment()), Dict::getComment, dict.getComment())
                .orderByDesc(Dict::getCreateTime)
        );
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
            return dictMapper.selectById(dict.getId());
        }
        List<Dict> dictList = dictMapper.selectList(Wrappers.<Dict>lambdaQuery()
                .eq(Dict::getDictType, dict.getDictType())
                .eq(StringUtils.isNotBlank(dict.getDictKey()), Dict::getDictKey, dict.getDictKey())
                .eq(StringUtils.isNotBlank(dict.getComment()), Dict::getComment, dict.getComment())
                .orderByDesc(Dict::getCreateTime)
        );
        dict = null;
        if (CollectionUtils.isNotEmpty(dictList)) {
            dict = dictList.get(0);
        }
        return dict;
    }

    @Override
    public List<Dict> selectLatestListDict(Dict dict) {
       return dictMapper.selectList(Wrappers.<Dict>lambdaQuery()
                .eq(Dict::getDictType, dict.getDictType())
                .eq(StringUtils.isNotBlank(dict.getDictKey()), Dict::getDictKey, dict.getDictKey())
                .eq(StringUtils.isNotBlank(dict.getComment()), Dict::getComment, dict.getComment())
                .orderByDesc(Dict::getCreateTime)
        );
    }

    @Override
    public List<Dict> selectUnExecutedTask() {
       return dictMapper.selectList(Wrappers.<Dict>lambdaQuery()
                .eq(Dict::getDictType, DictTypeEnum.FOLIB_UPGRADE_TASK.getType())
                .in(Dict::getComment, Lists.newArrayList(UpgradeTaskStatusEnum.UN_EXECUTED.getStatus(), UpgradeTaskStatusEnum.EXECUTED_FAIL.getStatus()))
                .orderByAsc(Dict::getCreateTime)
        );
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void updateUnExecutedTask(Dict dict) {
        dictMapper.update(Dict.builder().comment(dict.getComment()).build(), Wrappers.<Dict>lambdaUpdate()
                .eq(Dict::getDictType, dict.getDictType())
                .eq(Dict::getDictKey, dict.getDictKey())
                .eq(Dict::getDictValue, dict.getDictValue())
        );
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

    @Override
    public void updateById(Dict dict) {
        dictMapper.updateById(dict);
    }

    @Override
    public Dict getById(Long id) {
        return dictMapper.selectById(id);
    }

    public void  saveOrUpdateByTypeAndKey(Dict dict){
        List<Dict> dicts = dictMapper.selectList(Wrappers.<Dict>lambdaQuery()
                .eq(Dict::getDictType, dict.getDictType())
                .eq(Dict::getDictKey, dict.getDictKey()));
        if(dicts.isEmpty()){
            dictMapper.insert(dict);
        }else {
            dictMapper.update(dict,Wrappers.<Dict>lambdaUpdate().eq(Dict::getId, dict.getId()).eq(Dict::getDictType, dict.getDictType()));
        }
    }

    public List<Dict> selectByTypeAndKey(String type,String key){
        return dictMapper.selectList(Wrappers.<Dict>lambdaQuery()
                .eq(Dict::getDictType, type)
                .eq(StringUtils.isNotBlank(key),Dict::getDictKey, key));
    }

    @Override
    public TableResultResponse<Dict> getSystemDict(Integer page, Integer limit,String dictKey){
        PageHelper.startPage(page, limit);
        List<Dict> systems = selectByTypeAndKey(DictTypeEnum.SYSTEM_PROPERTY.getType(),dictKey);
        PageInfo<Dict> pages = PageInfo.of(systems);
        return new TableResultResponse<>(pages.getTotal(), pages.getList());
    }


}
