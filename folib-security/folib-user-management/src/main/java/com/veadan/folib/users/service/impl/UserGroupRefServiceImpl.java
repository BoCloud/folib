package com.veadan.folib.users.service.impl;

import com.veadan.folib.entity.UserGroupRef;
import com.veadan.folib.mapper.UserGroupRefMapper;
import com.veadan.folib.users.service.UserGroupRefService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;

import java.util.List;

/**
 * 用户组关联表;(user_group_ref)表服务实现类
 * @author : Fengmaogen
 * @date : 2024-7-18
 */
@Service
public class UserGroupRefServiceImpl implements UserGroupRefService {
    @Autowired
    private UserGroupRefMapper userGroupRefMapper;
    
    /** 
     * 通过ID查询单条数据 
     *
     * @param id 主键
     * @return 实例对象
     */
    public UserGroupRef queryById(Long id){
        return userGroupRefMapper.queryById(id);
    }
    
    /** 
     * 分页查询
     *
     * @param userGroupRef 筛选条件
     * @param pageRequest 分页对象
     * @return 查询结果
     */
    public Page<UserGroupRef> paginQuery(UserGroupRef userGroupRef, PageRequest pageRequest){
        long total = userGroupRefMapper.count(userGroupRef);
        return new PageImpl<>(userGroupRefMapper.queryAllByLimit(userGroupRef, pageRequest), pageRequest, total);
    }
    
    /** 
     * 新增数据
     *
     * @param userGroupRef 实例对象
     * @return 实例对象
     */
    public UserGroupRef insert(UserGroupRef userGroupRef){
        userGroupRefMapper.insert(userGroupRef);
        return userGroupRef;
    }
    
    /** 
     * 更新数据
     *
     * @param userGroupRef 实例对象
     * @return 实例对象
     */
    public UserGroupRef update(UserGroupRef userGroupRef){
        userGroupRefMapper.update(userGroupRef);
        return queryById(userGroupRef.getId());
    }
    
    /** 
     * 通过主键删除数据
     *
     * @param id 主键
     * @return 是否成功
     */
    public boolean deleteById(Long id){
        int total = userGroupRefMapper.deleteById(id);
        return total > 0;
    }

    /**
     * 批量保存用户组关系
     * @param entities
     * @return
     */
    public int saveBath(List<UserGroupRef> entities) {
        return userGroupRefMapper.insertBatch(entities);
    }
}