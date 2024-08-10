package com.veadan.folib.users.service;

import com.veadan.folib.authorization.dto.Role;
import com.veadan.folib.domain.User;
import com.veadan.folib.domain.UserEntity;
import com.veadan.folib.dto.RepositoryPrivilegeDTO;
import com.veadan.folib.dto.UserDTO;
import com.veadan.folib.entity.FolibUser;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;

import java.util.List;
import java.util.Set;

/**
  * @Description: 用户服务
  * @auther: fengmaogen
  * @CreateDate: 2024/7/9 21:16
  */
public interface FolibUserService {


    void deleteByUserName(String username);

    UserDTO findByUserName(String username);

    UserEntity save(UserEntity userEntity);

    Iterable<User> findAll();

    Long countUsers(User user);

    List<User> findUsersPage(User user, int start, Integer limit);

    boolean saveOrUpdateBatch(List<UserEntity> userEntitys);

    Set<Role> queryRoles(String uuid);

    void saveOrUpdate(List<FolibUser> users);

    Page<FolibUser> paginQuery(FolibUser build, PageRequest pageRequest);

    List<User> queryUserRoleByRepositoryAndPrivilege(List<RepositoryPrivilegeDTO> repositoryPrivilegeDTOS);
}
