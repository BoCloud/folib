package com.veadan.folib.users.service;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import com.veadan.folib.entity.Resource;

import java.util.List;

/**
 * 资源表;(resource)表服务接口
 * @author : Fengmaogen
 * @date : 2024-7-17
 */
public interface ResourceService{
    /** 
     * 通过ID查询单条数据 
     *
     * @param id 主键
     * @return 实例对象
     */
    Resource queryById(String id);
    /** 
     * 分页查询
     *
     * @param resource 筛选条件
     * @param pageRequest 分页对象
     * @return 查询结果
     */
    Page<Resource> paginQuery(Resource resource, PageRequest pageRequest);
    /** 
     * 新增数据
     *
     * @param resource 实例对象
     * @return 实例对象
     */
    Resource insert(Resource resource);
    /** 
     * 更新数据
     *
     * @param resource 实例对象
     * @return 实例对象
     */
    Resource update(Resource resource);
    /** 
     * 通过主键删除数据
     *
     * @param id 主键
     * @return 是否成功
     */
    boolean deleteById(String id);

     int saveBatch(List<Resource> collect);

    Resource queryResource(Resource build);
    List<Resource> queryResourceList(Resource resource);
    List<Resource> findAll();

    List<Resource> findResources(List<Resource> resources);

    void saveOrUpdateBatch(List<Resource> resources);

    List<Resource> queryByIds(List<String> resourceIds);
}