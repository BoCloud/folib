package com.veadan.folib.users.service.impl;

import com.veadan.folib.authorization.dto.Role;
import com.veadan.folib.constant.GlobalConstants;
import com.veadan.folib.converts.UserConvert;
import com.veadan.folib.domain.User;
import com.veadan.folib.domain.UserEntity;
import com.veadan.folib.dto.UserDTO;
import com.veadan.folib.entity.FolibUser;
import com.veadan.folib.mapper.FolibUserMapper;
import com.veadan.folib.dto.RepositoryPrivilegeDTO;
import com.veadan.folib.users.service.FolibUserService;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.BeanUtils;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.stereotype.Component;

import javax.inject.Inject;
import javax.transaction.Transactional;
import java.util.*;

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

    @Override
    public void deleteByUserName(String username) {
        FolibUser user = FolibUser.builder().username(username).deleted(GlobalConstants.NOT_DELETED).build();
        folibUserMapper.update(user);
    }

    @Override
    public UserDTO findByUserName(String username) {
        FolibUser user = FolibUser.builder().id(username).build();
        List<UserDTO> folibUser = folibUserMapper.queryUser(user);
        if (folibUser.isEmpty()) {
            return null;
        }
        return folibUser.get(0);
    }

    @Override
    public UserEntity save(UserEntity userEntity) {
        FolibUser folibUser = UserConvert.INSTANCE.UserEntityToFolibUser(userEntity);
        Date date = new Date();
        folibUser.setUpdateTime(date);
        FolibUser folibUserInfo = folibUserMapper.selectOne(FolibUser.builder().id(folibUser.getId()).build());
        if (Objects.equals(folibUserInfo, null)) {
            folibUser.setCreateTime(date);
            folibUserMapper.insert(folibUser);
        }else {
            folibUser.setId(folibUserInfo.getId());
            folibUserMapper.update(folibUser);
        }
        return userEntity;
    }

    @Override
    public Iterable<User> findAll() {
        List<UserDTO> folibUsers = folibUserMapper.queryUser(null);
        return UserConvert.INSTANCE.UserDTOsToUsers(folibUsers);
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
        if (Objects.isNull(limit)) {
            limit = 10;
        }
        FolibUser folibUser = new FolibUser();
        BeanUtils.copyProperties(user, folibUser);
        PageRequest pageRequest = PageRequest.of(start, limit);
        List<UserDTO> folibUsers = folibUserMapper.queryAllUserRoleByLimit(folibUser, pageRequest);
        List<UserEntity> userEntities = UserConvert.INSTANCE.UserDTOsToUserList(folibUsers);
        return new ArrayList<>(userEntities);
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
