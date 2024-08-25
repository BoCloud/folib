package com.veadan.folib.users.service.impl;

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
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.stereotype.Component;
import tk.mybatis.mapper.entity.Example;

import javax.inject.Inject;
import javax.transaction.Transactional;
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
        FolibUser user = FolibUser.builder().username(username).deleted(GlobalConstants.DELETED).build();
        folibUserMapper.update(user);

        userGroupRefService.deleteByUserId(username);

        roleResourceRefService.deleteByentityId(username, GlobalConstants.ROLE_TYPE_USER);
    }

    @Override
    public UserDTO findByUserName(String username) {
        List<UserDTO> folibUsers = getUsers(UserDto.builder().username(username).build(), 0, 1);
        List<UserEntity> userEntities = UserConvert.INSTANCE.UserDTOsToUserList(folibUsers);
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
        Date date = new Date();
        folibUser.setUpdateTime(date);
        FolibUser folibUserInfo = folibUserMapper.selectOne(FolibUser.builder().id(folibUser.getId()).build());
        if (Objects.equals(folibUserInfo, null)) {
            folibUserMapper.insert(folibUser);
        }else {
            folibUser.setDeleted(GlobalConstants.NOT_DELETED);
            folibUserMapper.update(folibUser);
        }
        return userEntity;
    }

    @Override
    public Iterable<User> findAll() {
        List<UserDTO> folibUsers = getUsers(UserDto.builder().build(), 0, null);
        List<UserEntity> userEntities = UserConvert.INSTANCE.UserDTOsToUserList(folibUsers);
        return new ArrayList<>(userEntities);
    }

    @Override
    public Long countUsers(User user) {
        FolibUser.FolibUserBuilder builder = FolibUser.builder();
        if (Objects.equals(user, null)) {
            if(StringUtils.isNoneBlank(user.getUsername())) {
                builder.username(user.getUsername());
            }
            if(StringUtils.isNoneBlank(user.getEmail())) {
                builder.email(user.getEmail());
            }
        }
        builder.deleted(GlobalConstants.NOT_DELETED);
        FolibUser folibUser = builder.build();
        return folibUserMapper.count(folibUser);
    }

    @Override
    public List<User> findUsersPage(User user, int start, Integer limit) {
        List<UserDTO> folibUsers = getUsers(user, start, limit);
        List<UserEntity> userEntities = UserConvert.INSTANCE.UserDTOsToUserList(folibUsers);
        return new ArrayList<>(userEntities);
    }

    @Override
    public List<UserDTO> getUsers(User user, int start, Integer limit) {
        if (Objects.isNull(limit)) {
            long count = countUsers(user);
            if (count == 0L) {
                return null;
            }
            limit = Math.toIntExact(count);
        }
        FolibUser folibUser = new FolibUser();
        BeanUtils.copyProperties(user, folibUser);
        if (CollectionUtils.isNotEmpty(user.getRoles())) {
            folibUser.setRoles(user.getRoles().stream().map(SecurityRole::getRoleName).collect(Collectors.toSet()));
        }
        PageRequest pageRequest = PageRequest.of(start, limit);
        List<UserDTO> folibUsers = folibUserMapper.queryAllUserRoleByLimit(folibUser, pageRequest);
        //获取用户权限
        getUserAuthorities(folibUsers);
        return folibUsers;
    }

    /**
     * 获取用户权限
     * @param folibUsers
     */
    private void getUserAuthorities(List<UserDTO> folibUsers) {
        if (CollectionUtils.isNotEmpty(folibUsers)) {
            List<String> roleIds = folibUsers.stream().flatMap(userDTO -> userDTO.getRoles().stream()).distinct().collect(Collectors.toList());
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
                                    if(CollectionUtils.isNotEmpty(userGroupAuthorities)) userAuthorities.addAll(userGroupAuthorities);
                        }

                        userDTO.setAuthorities(userAuthorities);
                    });
                }
            });
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
    public Page<FolibUser> paginQuery(FolibUser folibUser, PageRequest pageRequest) {
        long total = folibUserMapper.count(folibUser);
        return new PageImpl<>(folibUserMapper.queryAllByLimit(folibUser, pageRequest), pageRequest, total);

    }

    @Override
    public List<User> queryUserRoleByRepositoryAndPrivilege(List<RepositoryPrivilegeDTO> repositoryPrivilegeDTOS) {
        List<UserDTO> users = folibUserMapper.queryUserRoleByRepositoryAndPrivilege(repositoryPrivilegeDTOS);
        List<UserEntity> userEntities = UserConvert.INSTANCE.UserDTOsToUserList(users);
        return new ArrayList<>(userEntities);
    }

}
