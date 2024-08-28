package com.veadan.folib.users.service.impl;

import com.veadan.folib.dto.RoleResourceRefDTO;
import com.veadan.folib.entity.UserGroup;
import com.veadan.folib.entity.UserGroupRef;
import com.veadan.folib.mapper.UserGroupRefMapper;
import com.veadan.folib.users.service.UserGroupRefService;
import com.veadan.folib.users.service.UserGroupService;
import org.parboiled.common.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.transaction.annotation.Transactional;
import tk.mybatis.mapper.entity.Example;

import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

/**
 * 用户组关联表;(user_group_ref)表服务实现类
 * @author : Fengmaogen
 * @date : 2024-7-18
 */
@Service
@Transactional
public class UserGroupRefServiceImpl implements UserGroupRefService {
    @Autowired
    private UserGroupRefMapper userGroupRefMapper;
    @Autowired
    private UserGroupService userGroupService;
    
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
        updateGroupName(Collections.singletonList(userGroupRef));
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
        updateGroupName(Collections.singletonList(userGroupRef));
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
        updateGroupName(entities);
        return userGroupRefMapper.insertBatch(entities);
    }

    private void updateGroupName(List<UserGroupRef> entities) {
        List<Long> groupIds = entities.stream().filter(userGroupRef -> userGroupRef.getUserGroupName() == null).map(UserGroupRef::getUserGroupId).collect(Collectors.toList());
        if (!groupIds.isEmpty()) {
            List<UserGroup> userGroups = userGroupService.queryByIds(groupIds);
            Map<Long, String> nameMap = userGroups.stream().collect(Collectors.toMap(UserGroup::getId, UserGroup::getGroupName));
            entities.forEach(userGroupRef -> {
                    if (StringUtils.isEmpty(userGroupRef.getUserGroupName())) {
                        userGroupRef.setUserGroupName(nameMap.get(userGroupRef.getUserGroupId()));
                    }
            });
        }
    }

    @Override
    public List<RoleResourceRefDTO> queryPrivilegeByGroup(Long groupId, String refType, List<String> roleIds) {
        return userGroupRefMapper.queryPrivilegeByGroup(groupId, refType, roleIds);
    }

    @Override
    public void deleteByUserGroupId(Long id) {
        userGroupRefMapper.delete(UserGroupRef.builder().userGroupId(id).build());
    }

    @Override
    public void deleteByUserId(String userId) {
        userGroupRefMapper.delete(UserGroupRef.builder().userId(userId).build());
    }

    @Override
    public void batchUpdate(List<UserGroupRef> userGroups) {
        List<Long> groupIds = userGroups.stream().map(UserGroupRef::getUserGroupId).collect(Collectors.toList());
        List<UserGroupRef> queryUserGroupRefs = userGroupRefMapper.queryByGroupIds(groupIds);
        if (!queryUserGroupRefs.isEmpty()) {
            userGroups = userGroups.stream().filter(userGroupRef -> queryUserGroupRefs.stream().noneMatch(queryUserGroupRef -> queryUserGroupRef.getUserGroupId().equals(userGroupRef.getUserGroupId()) && queryUserGroupRef.getUserId().equals(userGroupRef.getUserId()))).collect(Collectors.toList());
        }
        if (!userGroups.isEmpty()) {
            userGroupRefMapper.insertBatch(userGroups);
        }
    }

    @Override
    public List<UserGroupRef> queryByGroupIds(List<Long> groupIds) {
        return userGroupRefMapper.queryByGroupIds(groupIds);
    }

    @Override
    public List<UserGroupRef> queryByUserId(String userId) {
        Example example = Example.builder(UserGroupRef.class).build();
        example.createCriteria().andEqualTo("userId", userId);
        return userGroupRefMapper.selectByExample(example);
    }


}