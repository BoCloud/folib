package com.veadan.folib.users.service.impl;

import com.veadan.folib.components.IdGenerateUtils;
import com.veadan.folib.constant.GlobalConstants;
import com.veadan.folib.dto.UserGroupDTO;
import com.veadan.folib.dto.UserGroupListDTO;
import com.veadan.folib.entity.UserGroup;
import com.veadan.folib.mapper.UserGroupMapper;
import com.veadan.folib.users.service.UserGroupService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Lazy;
import org.springframework.stereotype.Service;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;

import javax.inject.Inject;
import java.util.List;

/**
 * 用户组;(user_group)表服务实现类
 * @author : Fengmaogen
 * @date : 2024-7-17
 */
@Service
public class UserGroupServiceImpl implements UserGroupService {
    @Autowired
    private UserGroupMapper userGroupMapper;
    @Inject
    @Lazy
    private IdGenerateUtils idGenerateUtils;

    /** 
     * 通过ID查询单条数据 
     *
     * @param id 主键
     * @return 实例对象
     */
    public UserGroup queryById(Long id){
        return userGroupMapper.queryById(id);
    }
    
    /** 
     * 分页查询
     *
     * @param userGroup 筛选条件
     * @param pageRequest 分页对象
     * @return 查询结果
     */
    public Page<UserGroupListDTO> paginQuery(UserGroup userGroup, PageRequest pageRequest){
        long total = userGroupMapper.count(userGroup);
        return new PageImpl<>(userGroupMapper.queryAllByLimit(userGroup, pageRequest), pageRequest, total);
    }
    
    /** 
     * 新增数据
     *
     * @param userGroup 实例对象
     * @return 实例对象
     */
    public UserGroup save(UserGroup userGroup){
        userGroup.setId(idGenerateUtils.generateId("userGroupId"));
        userGroupMapper.insert(userGroup);
        return userGroup;
    }
    
    /** 
     * 更新数据
     *
     * @param userGroup 实例对象
     * @return 实例对象
     */
    public UserGroup update(UserGroup userGroup){
        userGroupMapper.update(userGroup);
        return queryById(userGroup.getId());
    }
    
    /** 
     * 通过主键删除数据
     *
     * @param id 主键
     * @return 是否成功
     */
    public boolean deleteById(Long id){
        int update = userGroupMapper.update(UserGroup.builder().id(id).deleted(GlobalConstants.DELETED).build());
        return update > 0;
    }

    @Override
    public List<UserGroup> queryUserGroupList(UserGroup userGroup) {
        return userGroupMapper.select(userGroup);
    }

    @Override
    public List<UserGroup> findAll() {
        return userGroupMapper.select(UserGroup.builder().deleted(GlobalConstants.NOT_DELETED).build());
    }

    @Override
    public UserGroupDTO queryGroupDetailById(Long groupId) {
        return userGroupMapper.queryGroupDetailById(groupId);
    }

    @Override
    public void saveOrUpdateBatch(List<UserGroup> groups) {
        userGroupMapper.insertOrUpdateBatch(groups);
    }
}