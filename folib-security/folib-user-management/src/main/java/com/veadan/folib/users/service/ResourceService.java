package com.veadan.folib.users.service;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import com.veadan.folib.entity.Resource;

 /**
 * 资源表;(resource)表服务接口
 * @author : http://www.chiner.pro
 * @date : 2024-7-17
 */
public interface ResourceService{
    /** 
     * 通过ID查询单条数据 
     *
     * @param id 主键
     * @return 实例对象
     */
    Resource queryById(Long id);
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
    boolean deleteById(Long id);
}