package com.veadan.folib.users.service.impl;

import com.veadan.folib.components.IdGenerateUtils;
import com.veadan.folib.entity.Resource;
import com.veadan.folib.mapper.ResourceMapper;
import com.veadan.folib.users.service.ResourceService;
import org.parboiled.common.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Lazy;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.stereotype.Service;

import javax.inject.Inject;
import javax.transaction.Transactional;
import java.util.List;

/**
 * 资源表;(resource)表服务实现类
 * @author : Fengmaogen
 * @date : 2024-7-17
 */
@Service
@Transactional
public class ResourceServiceImpl implements ResourceService {
    @Autowired
    private ResourceMapper resourceMapper;
    @Inject
    @Lazy
    private IdGenerateUtils idGenerateUtils;
    
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
    public Page<Resource> paginQuery(Resource resource, PageRequest pageRequest){
        long total = resourceMapper.count(resource);
        return new PageImpl<>(resourceMapper.queryAllByLimit(resource, pageRequest), pageRequest, total);
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
                String storageId = resource.getStorageId();
                resourceId = (storageId+repositoryId.trim()+path.trim()).toUpperCase();
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
        return total > 0;
    }

    @Override
    public int saveBatch(List<Resource> resources) {
        resources.forEach(resource -> {
            String resourceId = resource.getId();
            if (StringUtils.isEmpty(resourceId)) {
                String apiAuthoritie = resource.getApiAuthoritie();
                if (apiAuthoritie != null) resourceId = apiAuthoritie.toUpperCase();

                if (StringUtils.isNotEmpty(resourceId)) {
                    String path = resource.getPath();
                    String repositoryId = resource.getRepositoryId();
                    String storageId = resource.getStorageId();
                    resourceId = (storageId+repositoryId.trim()+path.trim()).toUpperCase();
                }
                resource.setId(resourceId);
            }
        });
        return resourceMapper.insertBatch(resources);
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
}