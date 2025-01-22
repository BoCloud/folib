package com.veadan.folib.users.service.impl;

import com.github.pagehelper.PageHelper;
import com.github.pagehelper.PageInfo;
import com.veadan.folib.components.IdGenerateUtils;
import com.veadan.folib.constant.GlobalConstants;
import com.veadan.folib.dto.UserGroupDTO;
import com.veadan.folib.dto.UserGroupListDTO;
import com.veadan.folib.entity.UserGroup;
import com.veadan.folib.entity.UserGroupRef;
import com.veadan.folib.mapper.UserGroupMapper;
import com.veadan.folib.users.service.RoleResourceRefService;
import com.veadan.folib.users.service.UserGroupRefService;
import com.veadan.folib.users.service.UserGroupService;
import com.veadan.folib.utils.UserManageUtils;
import org.apache.commons.collections4.CollectionUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Lazy;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import tk.mybatis.mapper.entity.Example;

import javax.inject.Inject;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.stream.Collectors;

/**
 * 用户组;(user_group)表服务实现类
 * @author : Fengmaogen
 * @date : 2024-7-17
 */
@Service
@Transactional(rollbackFor=Exception.class)
public class UserGroupServiceImpl implements UserGroupService {
    @Autowired
    private UserGroupMapper userGroupMapper;
    @Inject
    @Lazy
    private IdGenerateUtils idGenerateUtils;
    @Autowired
    private UserGroupRefService userGroupRefService;
    @Autowired
    private RoleResourceRefService roleResourceRefService;

    /** 
     * 通过ID查询单条数据 
     *
     * @param id 主键
     * @return 实例对象
     */
    @Override
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
    @Override
    public PageInfo<UserGroupListDTO> paginQuery(UserGroup userGroup, PageRequest pageRequest){
        PageHelper.startPage(pageRequest.getPageNumber(), pageRequest.getPageSize());
        List<UserGroupListDTO> userGroupListDTOS = userGroupMapper.queryAllByLimit(userGroup);
        return new PageInfo<>(userGroupListDTOS);
    }

    @Override
    public PageInfo<UserGroupListDTO> pageQueryAndUserNumber(UserGroup userGroup, PageRequest pageRequest) {
        PageHelper.startPage(pageRequest.getPageNumber(), pageRequest.getPageSize());
        List<UserGroupListDTO> userGroupListDTOS = userGroupMapper.queryAllByUser(userGroup);
        return new PageInfo<>(userGroupListDTOS);
    }

    /** 
     * 新增数据
     *
     * @param userGroup 实例对象
     * @return 实例对象
     */
    @Override
    public UserGroup save(UserGroup userGroup){
        String groupName = userGroup.getGroupName();
        List<UserGroup> userGroups = queryByGroupNames(Collections.singletonList(groupName));
        if (CollectionUtils.isNotEmpty(userGroups) && userGroups.get(0).getGroupName().equals(groupName)) {
            throw new RuntimeException("UserGroupName is already");
        }
        userGroup.setCreateBy(UserManageUtils.getUsername());
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
    @Override
    public UserGroup update(UserGroup userGroup){
        String groupName = userGroup.getGroupName();
        List<UserGroup> userGroups = queryByGroupNames(Collections.singletonList(groupName));
        if (CollectionUtils.isNotEmpty(userGroups)
                && userGroups.get(0).getGroupName().equals(groupName)
                && !userGroups.get(0).getId().equals(userGroup.getId())) {
            throw new RuntimeException("UserGroupName is already");
        }
        userGroup.setUpdateBy(UserManageUtils.getUsername());
        userGroupMapper.update(userGroup);
        //批量更新用户组关联用户表中的用户组名称冗余字段
        batchUpdateRefGroupName(Collections.singletonList(userGroup.getId()));
        return queryById(userGroup.getId());
    }

    /**
     * 批量更新用户组名称
     * @param groupIds 用户组id
     */
    private void batchUpdateRefGroupName(List<Long> groupIds) {
        List<UserGroup> userGroups = queryByIds(groupIds);
        if (CollectionUtils.isNotEmpty(userGroups)) {
            Map<Long, String> groupNameMap = userGroups.stream().collect(Collectors.toMap(UserGroup::getId, UserGroup::getGroupName));
            List<UserGroupRef> userGroupRefs = userGroupRefService.queryByGroupIds(groupIds);
            if (CollectionUtils.isNotEmpty(userGroupRefs)) {
                List<UserGroupRef> updateRefs = userGroupRefs.stream().filter(userGroupRef -> !Objects.equals(groupNameMap.get(userGroupRef.getUserGroupId()), userGroupRef.getUserGroupName())).collect(Collectors.toList());
                if (CollectionUtils.isNotEmpty(updateRefs)) {
                    userGroupRefService.batchUpdate(updateRefs);
                }
            }
        }
    }

    /** 
     * 通过主键删除数据
     *
     * @param id 主键
     * @return 是否成功
     */
    @Override
    public boolean deleteById(Long id){
        int update = userGroupMapper.deleteById(id);
        //删除用户组关联用户
        userGroupRefService.deleteByUserGroupId(id);
        //删除角色关联用户组
        roleResourceRefService.deleteByentityId(String.valueOf(id), GlobalConstants.ROLE_TYPE_USER_GROUP);
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
        batchUpdateRefGroupName(groups.stream().map(UserGroup::getId).collect(Collectors.toList()));
    }

    @Override
    public List<UserGroup> queryByIds(List<Long> ids) {
        if (CollectionUtils.isEmpty(ids)) {
            return Collections.emptyList();
        }
        Example example = new Example(UserGroup.class);
        example.createCriteria().andIn("id", ids);
        return userGroupMapper.selectByExample(example);
    }

    @Override
    public List<UserGroup> queryByGroupNames(List<String> groupNames) {
        if (CollectionUtils.isEmpty(groupNames)) {
            return Collections.emptyList();
        }
        Example example = new Example(UserGroup.class);
        example.createCriteria().andIn("groupName", groupNames);
        return userGroupMapper.selectByExample(example);
    }
}