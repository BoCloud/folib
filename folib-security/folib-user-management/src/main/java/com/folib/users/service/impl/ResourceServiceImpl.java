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
package com.folib.users.service.impl;

import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.github.pagehelper.PageHelper;
import com.github.pagehelper.PageInfo;
import com.folib.entity.Resource;
import com.folib.mapper.ResourceMapper;
import com.folib.users.service.ResourceService;
import com.folib.users.service.RoleResourceRefService;
import lombok.extern.slf4j.Slf4j;
import org.hibernate.validator.internal.util.stereotypes.Lazy;
import org.parboiled.common.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.PageRequest;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * 资源表;(resource)表服务实现类
 * @author veadan
 * @date : 2024-7-17
 */
@Slf4j
@Service
@Transactional(rollbackFor=Exception.class)
public class ResourceServiceImpl implements ResourceService {
    @Autowired
    private ResourceMapper resourceMapper;

    @Lazy
    @Autowired
    private RoleResourceRefService roleResourceRefService;

    /** 
     * 通过ID查询单条数据 
     *
     * @param id 主键
     * @return 实例对象
     */
    public Resource queryById(String id){
        return resourceMapper.queryById(id);
    }
    
    /** 
     * 分页查询
     *
     * @param resource 筛选条件
     * @param pageRequest 分页对象
     * @return 查询结果
     */
    public PageInfo<Resource> paginQuery(Resource resource, PageRequest pageRequest){
        PageHelper.startPage(pageRequest.getPageNumber(), pageRequest.getPageSize());
        List<Resource> resources = resourceMapper.queryAllByLimit(resource);
        return new PageInfo<>(resources);
    }
    
    /** 
     * 新增数据
     *
     * @param resource 实例对象
     * @return 实例对象
     */
    public Resource insert(Resource resource){
        String resourceId = resource.getId();
        if (StringUtils.isEmpty(resourceId)) {
            String apiAuthoritie = resource.getApiAuthoritie();
            if (apiAuthoritie != null) resourceId = apiAuthoritie.toUpperCase();

            if (StringUtils.isEmpty(resourceId)) {
                 String path = resource.getPath();
                    String repositoryId = resource.getRepositoryId();
                    resourceId = resource.getStorageId();
                    if (StringUtils.isNotEmpty(repositoryId)) {
                        resourceId += "_" + repositoryId.trim();
                    }
                    if (StringUtils.isNotEmpty(path)) {
                        resourceId += "_" + path.trim();
                    }
            }
            resource.setId(resourceId);
        }

        resourceMapper.insert(resource);
        return resource;
    }
    
    /** 
     * 更新数据
     *
     * @param resource 实例对象
     * @return 实例对象
     */
    public Resource update(Resource resource){
        resourceMapper.update(resource);
        return queryById(resource.getId());
    }
    
    /** 
     * 通过主键删除数据
     *
     * @param id 主键
     * @return 是否成功
     */
    public boolean deleteById(String id){
        int total = resourceMapper.deleteById(id);
        //删除资源关联的权限信息
        roleResourceRefService.deleteByResourceIds(Collections.singletonList(id));
        return total > 0;
    }

    @Override
    public int saveBatch(List<Resource> resources) {
        List<Resource> resourceList = new ArrayList<>();
        try {
           resources.forEach(resource -> {
                String resourceId = resource.getId();
                if (StringUtils.isEmpty(resourceId)) {
                    String apiAuthoritie = resource.getApiAuthoritie();
                    if (apiAuthoritie != null) resourceId = apiAuthoritie.toUpperCase();

                    if (StringUtils.isEmpty(resourceId)) {
                        String path = resource.getPath();
                        String repositoryId = resource.getRepositoryId();
                        resourceId = resource.getStorageId();
                        if (StringUtils.isNotEmpty(repositoryId)) {
                            resourceId += "_" + repositoryId.trim();
                        }
                        if (StringUtils.isNotEmpty(path)) {
                            resourceId += "_" + path.trim();
                        }
                    }
                    resource.setId(resourceId.toUpperCase());
                }
            });
            resourceList.addAll(resources);
            return resourceMapper.insertBatch(resources);
        } catch (Exception e) {
            String sqlStr = "insert into resource(id,api_authoritie,storage_id,repository_id,path,create_by)\n" +
                    "        values ";
            StringBuilder body = new StringBuilder(sqlStr);
            for (Resource resource : resourceList) {
                body.append("('").append(resource.getId()).append("',").append("'").append(resource.getApiAuthoritie()).append("',").append("'").append(resource.getStorageId()).append("',").append("'").append(resource.getRepositoryId()).append("',").append("'").append(resource.getPath()).append("',").append("'").append(resource.getCreateBy()).append("'),");
            }
            body.append(");");
            log.error("添加资源失败------------>{}", body);
        }
        return 0;
    }

    @Override
    public Resource queryResource(Resource resource) {
       return resourceMapper.selectOne(Wrappers.<Resource>lambdaQuery()
                .eq(Resource::getId, resource.getId())
                .eq(Resource::getStorageId,resource.getStorageId()));
    }

    //@Override
    //public List<Resource> queryResourceList(Resource resource) {
    //    return resourceMapper.selectList(resource);
    //}


    @Override
    public List<Resource> findAll() {
       return resourceMapper.selectList(Wrappers.<Resource>lambdaQuery());
       // return resourceMapper.selectAll();
    }

    @Override
    public List<Resource> findResources(List<Resource> resources) {
        return resourceMapper.findResources(resources);
    }

    @Override
    public void saveOrUpdateBatch(List<Resource> resources) {
        resourceMapper.insertOrUpdateBatch(resources);
    }

    @Override
    public List<Resource> queryByIds(List<String> resourceIds) {
       return resourceMapper.selectList(Wrappers.<Resource>lambdaQuery().in(Resource::getId, resourceIds));
    }

    @Override
    public List<Resource> queryByStorageId(String storageId) {
       return resourceMapper.selectList(Wrappers.<Resource>lambdaQuery().eq(Resource::getStorageId, storageId));
    }

    @Override
    public void deleteByIds(List<String> resourceIds) {
        resourceMapper.selectList(Wrappers.<Resource>lambdaQuery().in(Resource::getId, resourceIds));
        //删除资源关联的权限信息
        roleResourceRefService.deleteByResourceIds(resourceIds);
    }
}