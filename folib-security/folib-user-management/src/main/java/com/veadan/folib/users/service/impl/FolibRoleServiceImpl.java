package com.veadan.folib.users.service.impl;

import com.veadan.folib.users.service.FolibRoleService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import com.veadan.folib.entity.FolibRole;
import com.veadan.folib.mapper.FolibRoleMapper;
 /**
 * 角色信息;(folib_role)表服务实现类
 * @author : http://www.chiner.pro
 * @date : 2024-7-17
 */
@Service
public class FolibRoleServiceImpl implements FolibRoleService {
    @Autowired
    private FolibRoleMapper folibRoleMapper;
    
    /** 
     * 通过ID查询单条数据 
     *
     * @param id 主键
     * @return 实例对象
     */
    public FolibRole queryById(String id){
        return folibRoleMapper.queryById(id);
    }
    
    /** 
     * 分页查询
     *
     * @param folibRole 筛选条件
     * @param pageRequest 分页对象
     * @return 查询结果
     */
    public Page<FolibRole> paginQuery(FolibRole folibRole, PageRequest pageRequest){
        long total = folibRoleMapper.count(folibRole);
        return new PageImpl<>(folibRoleMapper.queryAllByLimit(folibRole, pageRequest), pageRequest, total);
    }
    
    /** 
     * 新增数据
     *
     * @param folibRole 实例对象
     * @return 实例对象
     */
    public FolibRole insert(FolibRole folibRole){
        folibRoleMapper.insert(folibRole);
        return folibRole;
    }
    
    /** 
     * 更新数据
     *
     * @param folibRole 实例对象
     * @return 实例对象
     */
    public FolibRole update(FolibRole folibRole){
        folibRoleMapper.update(folibRole);
        return queryById(folibRole.getId());
    }
    
    /** 
     * 通过主键删除数据
     *
     * @param id 主键
     * @return 是否成功
     */
    public boolean deleteById(String id){
        int total = folibRoleMapper.deleteById(id);
        return total > 0;
    }
}