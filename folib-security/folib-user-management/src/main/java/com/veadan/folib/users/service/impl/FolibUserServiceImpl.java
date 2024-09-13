package com.veadan.folib.users.service.impl;

import com.github.pagehelper.PageHelper;
import com.github.pagehelper.PageInfo;
import com.veadan.folib.authorization.dto.Role;
import com.veadan.folib.constant.GlobalConstants;
import com.veadan.folib.converts.UserConvert;
import com.veadan.folib.domain.SecurityRole;
import com.veadan.folib.domain.User;
import com.veadan.folib.domain.UserEntity;
import com.veadan.folib.dto.RepositoryPrivilegeDTO;
import com.veadan.folib.dto.UserDTO;
import com.veadan.folib.entity.FolibUser;
import com.veadan.folib.entity.RoleResourceRef;
import com.veadan.folib.mapper.FolibUserMapper;
import com.veadan.folib.users.dto.UserDto;
import com.veadan.folib.users.service.FolibUserService;
import com.veadan.folib.users.service.RoleResourceRefService;
import com.veadan.folib.users.service.UserGroupRefService;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.PageRequest;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;
import tk.mybatis.mapper.entity.Example;

import javax.inject.Inject;
import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/**
 * @Author: fengmg
 * @Date: 2024/7/9 21:20
 * @Description:
 */
@Slf4j
@Component
@Transactional
public class FolibUserServiceImpl implements FolibUserService {

    @Inject
    private FolibUserMapper folibUserMapper;
    @Autowired
    private UserGroupRefService userGroupRefService;
    @Autowired
    private RoleResourceRefService roleResourceRefService;

    @Override
    public void deleteByUserName(String username) {
        folibUserMapper.deleteById(username);

        userGroupRefService.deleteByUserId(username);

        roleResourceRefService.deleteByentityId(username, GlobalConstants.ROLE_TYPE_USER);
    }

    @Override
    public UserDTO findByUserName(String username) {
        PageInfo<UserDTO> folibUsers = getUsers(UserDto.builder().id(username).build(), 0, 1);
        List<UserEntity> userEntities = UserConvert.INSTANCE.UserDTOsToUserList(folibUsers.getList());
        if (CollectionUtils.isNotEmpty(userEntities)) {
            return UserConvert.INSTANCE.UserEntityToUserDTO(userEntities.get(0));
        }
        return null;

    }

    @Override
    @Deprecated
    public List<UserDTO> findByUserNameResource(List<String> usernames, String storageId, String repositoryId, String path) {
        return folibUserMapper.queryUsersNameResource(usernames, storageId, repositoryId, path, null);
    }

    @Override
    public List<FolibUser> queryByIds(List<String> userIds) {
        Example example = new Example(FolibUser.class);
        example.createCriteria().andIn("id", userIds);
        return folibUserMapper.selectByExample(example);
    }

    @Override
    public UserEntity save(UserEntity userEntity) {
        FolibUser folibUser = UserConvert.INSTANCE.UserEntityToFolibUser(userEntity);
        FolibUser folibUserInfo = folibUserMapper.selectOne(FolibUser.builder().id(folibUser.getId()).build());
        if (Objects.equals(folibUserInfo, null)) {
            if (StringUtils.isBlank(folibUser.getSourceId()) || !"ldapUserDetailsService".equalsIgnoreCase(folibUser.getSourceId())) {
                folibUser.setSourceId("dataBaseUserDetailService");
            }
            folibUserMapper.insert(folibUser);
        }else {
            folibUser.setDeleted(GlobalConstants.NOT_DELETED);
            folibUserMapper.update(folibUser);
        }
        return userEntity;
    }

    @Override
    public Iterable<User> findAll() {
        long count = countUsers(UserDto.builder().build());
        if (count == 0L) {
            return null;
        }
        int limit = Math.toIntExact(count);
        PageInfo<UserDTO> folibUsers = getUsers(UserDto.builder().build(), 1, limit);
        List<UserEntity> userEntities = UserConvert.INSTANCE.UserDTOsToUserList(folibUsers.getList());
        return new ArrayList<>(userEntities);
    }

    @Override
    public Long countUsers(User user) {
        FolibUser folibUser = new FolibUser();
        BeanUtils.copyProperties(user, folibUser);
        if (CollectionUtils.isNotEmpty(user.getRoles())) {
            folibUser.setRoles(user.getRoles().stream().map(SecurityRole::getRoleName).collect(Collectors.toSet()));
        }
        return folibUserMapper.countUserRole(folibUser);
    }

    @Override
    public PageInfo<User> findUsersPage(User user, int start, Integer limit) {
        PageInfo<UserDTO> folibUsers = getUsers(user, start, limit);
        List<UserEntity> userEntities = UserConvert.INSTANCE.UserDTOsToUserList(folibUsers.getList());
        PageInfo<User> pageUser = new PageInfo<>(new ArrayList<>(userEntities));
        BeanUtils.copyProperties(folibUsers, pageUser, "list");
        return pageUser;
    }

    @Override
    public PageInfo<UserDTO> getUsers(User user, int start, Integer limit) {

        FolibUser folibUser = new FolibUser();
        BeanUtils.copyProperties(user, folibUser);
        if (CollectionUtils.isNotEmpty(user.getRoles())) {
            folibUser.setRoles(user.getRoles().stream().map(SecurityRole::getRoleName).collect(Collectors.toSet()));
        }
        PageHelper.startPage(start, limit);
        List<UserDTO> folibUsers = folibUserMapper.queryAllUserRoleByLimit(folibUser);
        PageInfo<UserDTO> pageUser = new PageInfo<>(folibUsers);
        //获取用户权限
        List<UserDTO> content = pageUser.getList();
        if (CollectionUtils.isNotEmpty(content)) {
            getUserAuthorities(content);
        }
        return pageUser;
    }

    /**
     * 获取用户权限
     * @param folibUsers
     */
    private void getUserAuthorities(List<UserDTO> folibUsers) {
        if (CollectionUtils.isNotEmpty(folibUsers)) {
            List<String> roleIds = folibUsers.stream().filter(userDTO -> CollectionUtils.isNotEmpty(userDTO.getRoles())).flatMap(userDTO -> userDTO.getRoles().stream()).distinct().collect(Collectors.toList());
            if (CollectionUtils.isNotEmpty(roleIds)) {
                List<RoleResourceRef> roleResourceRefs = roleResourceRefService.queryPermissionsByRoleIds(roleIds);
                Map<String, Map<String, Set<String>>> roleMap = roleResourceRefs.stream().collect(Collectors.groupingBy(RoleResourceRef::getRoleId,
                        Collectors.toMap(ref -> ref.getEntityId() + "_" + ref.getRefType(),
                                ref -> Stream.of(ref.getPathPrivilege(), ref.getRepositoryPrivilege(), ref.getStoragePrivilege()).
                                        filter(StringUtils::isNotBlank).map(String::trim).collect(Collectors.toSet()),
                                (a, b) -> {
                                    a.addAll(b);
                                    return a;
                                }
                        )));

                folibUsers.forEach(userDTO -> {
                    Set<String> roles = userDTO.getRoles();
                    if (CollectionUtils.isNotEmpty(roles)) {
                        roles.forEach(role -> {
                            Map<String, Set<String>> userMap = roleMap.get(role);
                            Set<String> userAuthorities = userMap.get(userDTO.getId() + "_" + GlobalConstants.ROLE_TYPE_USER);
                            userDTO.setAuthorities(userAuthorities);
                            Set<String> userGroupIds = userDTO.getUserGroupIds();
                            if (CollectionUtils.isNotEmpty(userGroupIds)) {
                                Set<String> userGroupAuthorities = userGroupIds.stream().map(userGroupId -> roleMap.get(userGroupId + "_" + GlobalConstants.ROLE_TYPE_USER_GROUP))
                                        .filter(Objects::nonNull)
                                        .flatMap(map -> map.values().stream())
                                        .flatMap(Set::stream)
                                        .filter(StringUtils::isNotBlank)
                                        .collect(Collectors.toSet());
                                if(CollectionUtils.isNotEmpty(userGroupAuthorities)) {
                                    userAuthorities.addAll(userGroupAuthorities);
                                }
                            }

                            userDTO.setAuthorities(userAuthorities);
                        });
                    }
                });
            }

        }
    }

    @Override
    public boolean saveOrUpdateBatch(List<UserEntity> userEntitys) {
        List<FolibUser> folibUsers = UserConvert.INSTANCE.UserEntitysToFolibuiltyList(userEntitys);
        int i = folibUserMapper.insertOrUpdateBatch(folibUsers);
        return i == userEntitys.size();
    }

    @Override
    public Set<Role> queryRoles(String uuid) {
//        return folibUserMapper.queryRoles(uuid);
        return null;
    }

    @Override
    public void saveOrUpdate(List<FolibUser> users) {
        folibUserMapper.insertOrUpdateBatch(users);
    }

    @Override
    public PageInfo<FolibUser> paginQuery(FolibUser folibUser, PageRequest pageRequest) {
        PageHelper.startPage(pageRequest.getPageNumber(), pageRequest.getPageSize());
        List<FolibUser> folibUsers = folibUserMapper.queryAllByLimit(folibUser);
        return new PageInfo<>(folibUsers);

    }

    @Override
    public List<User> queryUserRoleByRepositoryAndPrivilege(List<RepositoryPrivilegeDTO> repositoryPrivilegeDTOS) {
        List<UserDTO> users = folibUserMapper.queryUserRoleByRepositoryAndPrivilege(repositoryPrivilegeDTOS);
        List<UserEntity> userEntities = UserConvert.INSTANCE.UserDTOsToUserList(users);
        return new ArrayList<>(userEntities);
    }

}
