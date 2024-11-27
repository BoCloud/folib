package com.veadan.folib.users.service.impl;

import com.github.pagehelper.PageHelper;
import com.github.pagehelper.PageInfo;
import com.veadan.folib.entity.Resource;
import com.veadan.folib.mapper.ResourceMapper;
import com.veadan.folib.users.service.ResourceService;
import com.veadan.folib.users.service.RoleResourceRefService;
import lombok.extern.slf4j.Slf4j;
import org.parboiled.common.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.PageRequest;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import tk.mybatis.mapper.entity.Example;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * 资源表;(resource)表服务实现类
 * @author : Fengmaogen
 * @date : 2024-7-17
 */
@Slf4j
@Service
@Transactional
public class ResourceServiceImpl implements ResourceService {
    @Autowired
    private ResourceMapper resourceMapper;
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
        return resourceMapper.selectOne(resource);
    }

    @Override
    public List<Resource> queryResourceList(Resource resource) {
        return resourceMapper.select(resource);
    }


    @Override
    public List<Resource> findAll() {
        return resourceMapper.selectAll();
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
        Example example = new Example(Resource.class);
        example.createCriteria().andIn("id", resourceIds);
        return resourceMapper.selectByExample(example);
    }

    @Override
    public List<Resource> queryByStorageId(String storageId) {
        Example example = new Example(Resource.class);
        example.createCriteria().andEqualTo("storageId", storageId);
        return resourceMapper.selectByExample(example);
    }

    @Override
    public void deleteByIds(List<String> resourceIds) {
        Example example = new Example(Resource.class);
        example.createCriteria().andIn("id", resourceIds);
        resourceMapper.selectByExample(example);
        //删除资源关联的权限信息
        roleResourceRefService.deleteByResourceIds(resourceIds);
    }
}