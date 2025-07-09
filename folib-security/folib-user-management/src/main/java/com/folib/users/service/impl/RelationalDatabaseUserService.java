/*
 * Folib - [新一代AI制品仓库]
 * Copyright (C) 2025 bocloud.com.cn <folib@beyondcent.com>
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * 本程序是自由软件：您可依据GNU通用公共许可证（GPL-3.0+）条款重新发布和修改，
 * 但禁止任何形式的商业售卖行为（包括但不限于：直接销售、捆绑销售、云服务商用）。
 *
 * This program is distributed WITHOUT ANY WARRANTY.
 * Commercial sale of this software is expressly prohibited.
 *
 * For license details, see: https://www.gnu.org/licenses/gpl-3.0.html
 * 商业授权咨询请联系：folib@beyondcent.com
 */
package com.folib.users.service.impl;

import com.folib.users.service.*;
import com.github.pagehelper.PageInfo;
import com.folib.components.DistributedLockComponent;
import com.folib.constant.GlobalConstants;
import com.folib.converts.UserConvert;
import com.folib.data.CacheName;
import com.folib.domain.PageResultResponse;
import com.folib.domain.SecurityRole;
import com.folib.domain.User;
import com.folib.domain.UserEntity;
import com.folib.dto.PermissionsDTO;
import com.folib.dto.RepositoryPrivilegeDTO;
import com.folib.dto.UserDTO;
import com.folib.entity.FolibRole;
import com.folib.entity.RoleResourceRef;
import com.folib.entity.UserGroup;
import com.folib.entity.UserGroupRef;
import com.folib.repositories.UserRepository;
import com.folib.services.ConfigurationManagementService;
import com.folib.users.domain.SystemRole;
import com.folib.users.domain.Users;
import com.folib.users.dto.UserDto;
import com.folib.users.security.JwtAuthenticationClaimsProvider;
import com.folib.users.security.JwtClaimsProvider;
import com.folib.users.security.SecurityTokenProvider;
import com.folib.users.service.impl.RelationalDatabaseUserService.RelationalDatabase;
import com.folib.users.userdetails.SpringSecurityUser;
import com.folib.users.userdetails.UserDetailsMapper;
import com.folib.util.LocalDateTimeInstance;
import com.folib.util.RSAUtils;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.compress.utils.Lists;
import org.apache.commons.lang3.StringUtils;
import org.jose4j.lang.JoseException;
import org.springframework.cache.annotation.CacheEvict;
import org.springframework.context.annotation.Lazy;
import org.springframework.security.core.userdetails.UsernameNotFoundException;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;

import javax.inject.Inject;
import javax.inject.Qualifier;
import java.lang.annotation.Documented;
import java.lang.annotation.Retention;
import java.time.Instant;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.StreamSupport;

import static java.lang.annotation.RetentionPolicy.RUNTIME;

/**
 * @author veadan
 */
@Slf4j
@Component
@RelationalDatabase
@Transactional(rollbackFor = Exception.class)
public class RelationalDatabaseUserService implements UserService
{

    @Lazy
    @Inject
    private SecurityTokenProvider tokenProvider;
    @Inject
    private UserDetailsMapper userDetailsMapper;
    @Inject
    @JwtAuthenticationClaimsProvider.JwtAuthentication
    private JwtClaimsProvider jwtClaimsProvider;
    @Inject
    protected FolibUserService folibUserService;
    @Inject
    private UserGroupService userGroupService;
    @Inject
    private UserGroupRefService userGroupRefService;
    @Inject
    private RoleResourceRefService roleResourceRefService;
    @Inject
    protected UserRepository userRepository;
    @Lazy
    @Inject
    private FolibRoleService folibRoleService;
    @Inject
    private ResourceService resourceService;
    @Lazy
    @Inject
    private ConfigurationManagementService configurationManagementService;

    @Inject
    private DistributedLockComponent distributedLockComponent;
    @Inject
    private RSAUtils rsaUtils;
    @Inject
    private PasswordEncoder passwordEncoder;


    @Override
    @CacheEvict(cacheNames = CacheName.User.AUTHENTICATIONS, key = "#p0")
    public void deleteByUsername(String username)
    {
        folibUserService.deleteByUserName(username);
    }

    @Override
    public List<User> findUserByRoles(List<String> rolesList) {
        List<RepositoryPrivilegeDTO> repositoryPrivilegeDTOList = new ArrayList<>();
        rolesList.forEach(role -> {
            String[] split = role.split("\\|");
            repositoryPrivilegeDTOList.add(RepositoryPrivilegeDTO.builder().storage(split[0]).repository(split[1]).privilege(split[2]).build());
        });
        //通过存储id、仓库id、指定权限查询关联的用户
        return folibUserService.queryUserRoleByRepositoryAndPrivilege(repositoryPrivilegeDTOList);
    }


    @Override
    public UserEntity findByUsername(String username)
    {
        PageInfo<UserDTO> folibUsers = folibUserService.getUsers(UserDto.builder().id(username).build(), 1, 1);
        List<UserEntity> userEntities = UserConvert.INSTANCE.UserDTOsToUserList(folibUsers.getList());
        if (CollectionUtils.isNotEmpty(userEntities)) {
            return userEntities.get(0);
        }
        return null;
    }

    @Override
    public String generateSecurityToken(String username)
        throws JoseException
    {
        return generateSecurityToken(username, null);
    }

    @Override
    public String generateSecurityToken(String username, Integer expireSeconds) throws JoseException {
        final User user = findByUsername(username);
        SpringSecurityUser springSecurityUser = userDetailsMapper.apply(user);
        Map<String, String> claimMap = jwtClaimsProvider.getClaims(springSecurityUser);
        return tokenProvider.getToken(username, claimMap, expireSeconds, null);
    }

    @Override
    @CacheEvict(cacheNames = CacheName.User.AUTHENTICATIONS, key = "#p0.username")
    public void updateAccountDetailsByUsername(User userToUpdate)
    {
        UserEntity user = findByUsername(userToUpdate.getUsername());
        if (user == null)
        {
            throw new UsernameNotFoundException(userToUpdate.getUsername());
        }

        if (!StringUtils.isBlank(userToUpdate.getPassword()))
        {
            user.setPassword(userToUpdate.getPassword());
        }

        if (StringUtils.isNotBlank(userToUpdate.getOriginalPassword()))
        {
            user.setOriginalPassword(userToUpdate.getOriginalPassword());
        }

        if (StringUtils.isNotBlank(userToUpdate.getSecurityTokenKey()))
        {
            user.setSecurityTokenKey(userToUpdate.getSecurityTokenKey());
        }

        if (!StringUtils.isBlank(userToUpdate.getAvatar()))
        {
            user.setAvatar(userToUpdate.getAvatar());
        }

        if (!StringUtils.isBlank(userToUpdate.getEmail()))
        {
            user.setEmail(userToUpdate.getEmail());
        }
        save(user);
    }

    @Override
    public Users getUsers()
    {
        Iterable<User> users = folibUserService.findAll();
        return new Users(StreamSupport.stream(users.spliterator(), false).collect(Collectors.toSet()));
    }

    @Override
    public PageResultResponse<User> queryUser(User user, Integer page, Integer limit) {
        if (Objects.isNull(page) || page < 1) {
            page = 1;
        }
        if (Objects.isNull(limit)) {
            limit = 10;
        }
        PageInfo<User> usersPage = folibUserService.findUsersPage(user, page, limit);
        return new PageResultResponse<>(usersPage.getTotal(), usersPage.getList());
    }

    @Override
    public void revokeEveryone(String roleToRevoke)
    {
        //删除角色关联的用户、用户组
        List<RoleResourceRef> roleResourceRefs = roleResourceRefService.queryRefs(RoleResourceRef.builder().roleId(roleToRevoke).build());
        if (!CollectionUtils.isEmpty(roleResourceRefs)) {
            List<Long> refIds = roleResourceRefs.stream().map(RoleResourceRef::getId).collect(Collectors.toList());
            roleResourceRefService.removeByIds(refIds);
        }
    }

    @Override
    @CacheEvict(cacheNames = CacheName.User.AUTHENTICATIONS, key = "#p0.username")
    @Transactional(rollbackFor = Exception.class)
    public User save(User user)
    {
        LocalDateTime now = LocalDateTimeInstance.now();
        Date date = new Date();

        UserEntity userEntity = Optional.ofNullable(findByUsername(user.getUsername())).orElseGet(() -> new UserEntity(user.getUsername()));

        if (!StringUtils.isBlank(user.getPassword()))
        {
            userEntity.setPassword(user.getPassword());
        }
        if (StringUtils.isNotBlank(user.getOriginalPassword()))
        {
            userEntity.setOriginalPassword(user.getOriginalPassword());
        }
        if (StringUtils.isNotBlank(user.getSourceId()))
        {
            userEntity.setSourceId(user.getSourceId());
        }
        userEntity.setEnabled(user.isEnabled());

        userEntity.setUsername(user.getUsername());
        userEntity.setUuid(user.getUsername());
        userEntity.setSecurityTokenKey(user.getSecurityTokenKey());
        userEntity.setEmail(user.getEmail());
        userEntity.setLastUpdated(now);
        userEntity.setUserType("general");
        userEntity.setAvatar(user.getAvatar());
        userEntity.setNickname(user.getNickname());

        Set<SecurityRole> rolesList = user.getRoles();
        if ("admin".equalsIgnoreCase(user.getUuid()) && (!rolesList.stream().map(SecurityRole::getRoleName).collect(Collectors.toList()).contains(SystemRole.ADMIN.name())
        && !rolesList.stream().map(SecurityRole::getRoleName).collect(Collectors.toList()).contains(SystemRole.ADMIN.name().toLowerCase()))) {
            throw new IllegalArgumentException("admin user must have admin role");
        }

        //维护用户组
        Set<String> groupIds = user.getUserGroupIds();
        if (CollectionUtils.isEmpty(groupIds)) {
            if (!"admin".equalsIgnoreCase(user.getUuid())) {
                userGroupRefService.deleteByUserId(user.getUuid());
                List<UserGroup> userGroups = userGroupService.queryUserGroupList(UserGroup.builder().joinGroup(GlobalConstants.DEFALUT).deleted(GlobalConstants.NOT_DELETED).build());
                groupIds = Optional.ofNullable(userGroups).orElse(Lists.newArrayList()).stream().map(item -> String.valueOf(item.getId())).collect(Collectors.toSet());
            }
        }
        if (!CollectionUtils.isEmpty(groupIds)) {
            List<UserGroupRef> userGroupRefs = userGroupRefService.queryByUserId(user.getUuid());
            Set<String> finalGroupIds = groupIds;
            List<Long> removeGroupRefIds = userGroupRefs.stream().filter(item -> !finalGroupIds.contains(item.getUserGroupId().toString())).map(UserGroupRef::getId).collect(Collectors.toList());
            if (!CollectionUtils.isEmpty(removeGroupRefIds)) {
                userGroupRefService.deleteByIds(removeGroupRefIds);
            }
            List<String> userGroupIds = userGroupRefs.stream().map(UserGroupRef::getUserGroupId).map(String::valueOf).collect(Collectors.toList());
            userGroupIds.forEach(groupIds::remove);
            if (!CollectionUtils.isEmpty(groupIds)) {
                List<UserGroupRef> ref = new ArrayList<>();
                groupIds.forEach(item ->
                        ref.add(UserGroupRef.builder().userGroupId(Long.valueOf(item)).userId(user.getUuid()).createTime(date).build()));
                userGroupRefService.saveBath(ref);
            }
        }

        //维护用户角色
        Set<SecurityRole> roles = rolesList;
        if (!CollectionUtils.isEmpty(roles)){
             List<FolibRole> defaultRoles = folibRoleService.queryRoles(FolibRole.builder().isDefault(GlobalConstants.DEFALUT).build());
                if(!CollectionUtils.isEmpty(defaultRoles)) {
                    List<String> defaultRoleIds = defaultRoles.stream().map(FolibRole::getId).collect(Collectors.toList());
                    List<String> roleIds = roles.stream().filter(role -> defaultRoleIds.contains(role.getUuid())).map(SecurityRole::getRoleName).collect(Collectors.toList());
                    List<RoleResourceRef> roleResourceRefs = roleResourceRefService.queryRoleByUserId(user.getUuid(),defaultRoleIds);
                    List<String> userRoleIds = new ArrayList<>();
                    if (!CollectionUtils.isEmpty(roleResourceRefs)) {
                        List<Long> removeRoleResourceRefIds = roleResourceRefs.stream().filter(ref -> defaultRoleIds.contains(ref.getRoleId()) && !roleIds.contains(ref.getRoleId())).map(RoleResourceRef::getId).collect(Collectors.toList());
                        if (!CollectionUtils.isEmpty(removeRoleResourceRefIds)) {
                            roleResourceRefService.deleteByIds(removeRoleResourceRefIds);
                        }
                        userRoleIds = roleResourceRefs.stream().map(RoleResourceRef::getRoleId).collect(Collectors.toList());
                    }

                    List<String> finalUserRoleIds = userRoleIds;
                    List<String> addRoleIds = roleIds.stream().filter(roleId -> !finalUserRoleIds.contains(roleId)).collect(Collectors.toList());
                    if (!CollectionUtils.isEmpty(addRoleIds)) {
                        List<RoleResourceRef> resourceRefs = new ArrayList<>();
                        addRoleIds.forEach(roleId -> resourceRefs.add(RoleResourceRef.builder().roleId(roleId).refType(GlobalConstants.ROLE_TYPE_USER).entityId(user.getUuid()).build()));
                        roleResourceRefService.saveBath(resourceRefs);
                    }

                }
        }else {
            List<PermissionsDTO> permissions = roleResourceRefService.queryPermissions(null, user.getUuid(), null, null, false);
            List<Long> refIds = permissions.stream().filter(item -> SystemRole.ADMIN.name().equals(item.getRoleId()) || SystemRole.OPEN_SOURCE_MANAGE.name().equals(item.getRoleId())).map(PermissionsDTO::getId).collect(Collectors.toList());
            if (!CollectionUtils.isEmpty(refIds)) {
                roleResourceRefService.deleteByIds(refIds);
            }
        }
        //有权限变更，删除缓存
        folibRoleService.deleteUserRoleCache(Collections.singletonList(user.getUuid()));

        return folibUserService.save(userEntity);
    }

    @Override
    public User saveOverrideRole(User user) {
        LocalDateTime now = LocalDateTimeInstance.now();

        UserEntity userEntity = Optional.ofNullable(findByUsername(user.getUsername())).orElseGet(() -> new UserEntity(user.getUsername()));

        if (!StringUtils.isBlank(user.getPassword()))
        {
            userEntity.setPassword(user.getPassword());
        }
        userEntity.setEnabled(user.isEnabled());
        userEntity.setRoles(user.getRoles());
        userEntity.setSecurityTokenKey(user.getSecurityTokenKey());
        userEntity.setEmail(user.getEmail());
        userEntity.setLastUpdated(now);
        userEntity.setUserType("general");
        return folibUserService.save(userEntity);
    }

    public void expireUser(String username, boolean clearSourceId)
    {
        UserEntity externalUserEntry = findByUsername(username);
        externalUserEntry.setLastUpdated(LocalDateTime.ofInstant(Instant.ofEpochMilli(0), ZoneId.systemDefault()));

        if (clearSourceId)
        {
            externalUserEntry.setSourceId("empty");
        }

        folibUserService.save(externalUserEntry);
    }

    @Documented
    @Retention(RUNTIME)
    @Qualifier
    public @interface RelationalDatabase
    {
    }

    public boolean syncUser() {
        Iterable<User> users = userRepository.findAll();
        if (CollectionUtils.isEmpty((Collection<?>) users)) {

            UserDto user = UserDto.builder().username("admin").password("folib@v587").userType("general").enabled(true).roles(Collections.singleton(SystemRole.ADMIN.name())).build();

            String password = rsaUtils.encrypt(user.getPassword());
            user.setOriginalPassword(password);
            user.setPassword(passwordEncoder.encode(user.getPassword()));
            save(user);

            //清理管理员用户其他角色
            removeAdminUserOtherRoles(Collections.singletonList(user.getUsername()));
            return true;
        }
        List<User> userInfos = StreamSupport.stream(users.spliterator(), false).collect(Collectors.toList());

        List<UserEntity> userEntities = UserConvert.INSTANCE.UserListToUserEntityList(userInfos);
        //用户信息入库dataBaseUserDetailService
        userEntities.forEach(userEntity -> {
             if (StringUtils.isBlank(userEntity.getSourceId()) || !"ldapUserDetailsService".equals(userEntity.getSourceId())) {
                userEntity.setSourceId("dataBaseUserDetailService");
            }
        });
        folibUserService.saveOrUpdateBatch(userEntities);
        //处理用户关联的角色、用户添加默认组
        List<UserGroup> userGroups = userGroupService.queryUserGroupList(UserGroup.builder().joinGroup(GlobalConstants.DEFALUT).deleted(GlobalConstants.NOT_DELETED).build());
        if(CollectionUtils.isNotEmpty(userEntities)) {
            //用户组关系入库
            if(CollectionUtils.isNotEmpty(userGroups)){
                List<UserGroupRef> userGroupRefs = new ArrayList<>(userGroups.size()*userEntities.size());
                userGroups.forEach(userGroup -> {
                    userEntities.forEach(userEntity -> {
                        userGroupRefs.add(UserGroupRef.builder().userGroupId(userGroup.getId()).userId(userEntity.getUuid()).build());
                    });
                });
                userGroupRefService.saveBath(userGroupRefs);
            }
            //用户权限入库
            List<RoleResourceRef> roleResourceRefs = new ArrayList<>(userEntities.size());
            userEntities.forEach(userEntity -> {
                Set<SecurityRole> roles = userEntity.getRoles();
                if(CollectionUtils.isNotEmpty(roles)){
                    roles.forEach(securityRole -> {
                        roleResourceRefs.add(RoleResourceRef.builder().roleId(securityRole.getUuid()).entityId(userEntity.getUuid()).refType(GlobalConstants.ROLE_TYPE_USER).build());
                    });
                }
            });
            roleResourceRefService.saveBath(roleResourceRefs);
        }
        //有admin权限，断开存储空间角色和用户关系
        List<String> userIds = userInfos.stream().map(user -> {
            if (user.getRoles().stream().map(SecurityRole::getUuid).collect(Collectors.toSet()).contains(SystemRole.ADMIN.name())) {
                return user.getUuid();
            }
            return null;
        }).filter(Objects::nonNull).collect(Collectors.toList());
        log.info("remove admin user other roles, userIds: {}", userIds);
        //清理管理员用户其他角色
        removeAdminUserOtherRoles(userIds);

        return true;
    }

    private void removeAdminUserOtherRoles(List<String> userIds) {
        if (!CollectionUtils.isEmpty(userIds)) {
            List<RoleResourceRef> roleResourceRefs = roleResourceRefService.queryByUserIds(userIds);
            List<Long> refIds = roleResourceRefs.stream().filter(ref -> !SystemRole.ADMIN.name().equalsIgnoreCase(ref.getRoleId())).map(RoleResourceRef::getId).collect(Collectors.toList());
            log.info("remove admin user other roles, refIds: {}", refIds);
            if (!CollectionUtils.isEmpty(refIds)) {
                roleResourceRefService.deleteByIds(refIds);
            }
        }
    }

}
