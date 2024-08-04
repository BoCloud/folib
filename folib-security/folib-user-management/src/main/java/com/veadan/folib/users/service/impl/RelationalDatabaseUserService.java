package com.veadan.folib.users.service.impl;

import com.google.common.collect.Lists;
import com.google.common.collect.Sets;
import com.veadan.folib.constant.GlobalConstants;
import com.veadan.folib.converts.UserConvert;
import com.veadan.folib.data.CacheName;
import com.veadan.folib.domain.PageResultResponse;
import com.veadan.folib.domain.SecurityRole;
import com.veadan.folib.domain.User;
import com.veadan.folib.domain.UserEntity;
import com.veadan.folib.dto.UserDTO;
import com.veadan.folib.entity.*;
import com.veadan.folib.repositories.UserRepository;
import com.veadan.folib.users.domain.SystemRole;
import com.veadan.folib.users.domain.Users;
import com.veadan.folib.users.security.JwtAuthenticationClaimsProvider;
import com.veadan.folib.users.security.JwtClaimsProvider;
import com.veadan.folib.users.security.SecurityTokenProvider;
import com.veadan.folib.users.service.*;
import com.veadan.folib.users.userdetails.SpringSecurityUser;
import com.veadan.folib.users.userdetails.UserDetailsMapper;
import com.veadan.folib.util.LocalDateTimeInstance;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.jose4j.lang.JoseException;
import org.springframework.beans.BeanUtils;
import org.springframework.cache.annotation.CacheEvict;
import org.springframework.security.core.userdetails.UsernameNotFoundException;
import org.springframework.stereotype.Component;
import com.veadan.folib.users.service.impl.RelationalDatabaseUserService.RelationalDatabase;

import javax.inject.Inject;
import javax.inject.Qualifier;
import javax.transaction.Transactional;
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
 * @author xuxinping
 */
@Component
@RelationalDatabase
@Transactional
public class RelationalDatabaseUserService implements UserService
{

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
    @Inject
    private FolibRoleService folibRoleService;

    @Override
    @CacheEvict(cacheNames = CacheName.User.AUTHENTICATIONS, key = "#p0")
    public void deleteByUsername(String username)
    {
        folibUserService.deleteByUserName(username);
    }

    @Override
    public List<User> findUserByRoles(List<String> rolesList) {
        //FIXME 通过存储id、仓库id、指定权限查询关联的用户
        return null;
    }

    @Override
    public UserEntity findByUsername(String username)
    {
        UserDTO byUserName = folibUserService.findByUserName(username);
        return UserConvert.INSTANCE.UserDTOToUserEntity(byUserName);
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
        if (Objects.isNull(page)) {
            page = 1;
        }
        if (Objects.isNull(limit)) {
            limit = 5;
        }
        int start = (page - 1) * limit;
        limit = page * limit;
        long count = folibUserService.countUsers(user);
        if (count == 0L) {
            return null;
        }
        List<User> userList = folibUserService.findUsersPage(user, start, limit);
        return new PageResultResponse<User>(count, userList);
    }

    @Override
    public void revokeEveryone(String roleToRevoke)
    {
        //FIXME 删除角色关联的用户、用户组
    }

    @Override
    @CacheEvict(cacheNames = CacheName.User.AUTHENTICATIONS, key = "#p0.username")
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
        userEntity.setEnabled(user.isEnabled());

        userEntity.setSecurityTokenKey(user.getSecurityTokenKey());
        userEntity.setEmail(user.getEmail());
        userEntity.setLastUpdated(now);
        userEntity.setUserType("general");
        userEntity.setAvatar(user.getAvatar());

        //维护用户组
        Set<Long> groupIds = user.getGroupIds();
        if (!CollectionUtils.isEmpty(groupIds)) {
            userGroupRefService.deleteByUserId(user.getUuid());
            List<UserGroupRef> ref = new ArrayList<>();
            groupIds.forEach(item ->
                    ref.add(UserGroupRef.builder().userGroupId(item).userId(user.getUuid()).createTime(date).build()));
            userGroupRefService.saveBath(ref);
        }
        //维护用户角色
        Set<SecurityRole> roles = user.getRoles();
        if (!CollectionUtils.isEmpty(roles)){
            List<FolibRole> defaultRoles = folibRoleService.queryRoles(FolibRole.builder().isDefault(GlobalConstants.DEFALUT).build());
            if(!CollectionUtils.isEmpty(defaultRoles)) {
                List<RoleResourceRef> roleResourceRefs = roleResourceRefService.queryRoleByUserId(user.getUuid(), defaultRoles.stream().map(FolibRole::getId).collect(Collectors.toList()));
                if (!CollectionUtils.isEmpty(roleResourceRefs)) {
                    roleResourceRefService.removeByIds(roleResourceRefs.stream().map(RoleResourceRef::getId).collect(Collectors.toList()));
                }
            }

            List<RoleResourceRef> resourceRefs = new ArrayList<>();
            roles.forEach(role -> {
                resourceRefs.add(RoleResourceRef.builder().roleId(role.getRoleName()).refType(GlobalConstants.ROLE_TYPE_USER).entityId(user.getUuid()).createTime(date).build());
            });
            roleResourceRefService.saveBath(resourceRefs);
        }

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
        List<User> userInfos = StreamSupport.stream(users.spliterator(), false).collect(Collectors.toList());

        List<UserEntity> userEntities = UserConvert.INSTANCE.UserListToUserEntityList(userInfos);
        //用户信息入库
        folibUserService.saveOrUpdateBatch(userEntities);
        //FIXME 处理用户关联的角色、用户添加默认组
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

        return true;
    }

}
