package com.veadan.folib.users.service;

import com.veadan.folib.domain.PageResultResponse;
import com.veadan.folib.domain.User;
import com.veadan.folib.users.domain.Users;
import org.jose4j.lang.JoseException;

import java.util.List;

/**
 * @author
 * @author veadan
 */
public interface UserService {

    User findByUsername(String username);

    /**
     * Generates another one 'Security Token' for specific user.<br>
     * Token will be based on 'username' with 'securityTokenKey' used as clam.
     *
     * @param username user ID
     * @return encrypted token
     * @throws JoseException
     */
    String generateSecurityToken(String username)
            throws JoseException;

    /**
     * Generates another one 'Security Token' for specific user.<br>
     * Token will be based on 'username' with 'securityTokenKey' used as clam.
     *
     * @param username      user ID
     * @param expireSeconds expire Seconds
     * @return encrypted token
     * @throws JoseException
     */
    String generateSecurityToken(String username, Integer expireSeconds)
            throws JoseException;

    /**
     * This method is mainly necessary for the UI - for users to be able to update their own account data
     * (i.e. change password or securityToken)
     *
     * @param userToUpdate
     */
    void updateAccountDetailsByUsername(User userToUpdate);

    Users getUsers();

    PageResultResponse<User> queryUser(User user, Integer page, Integer limit);

    void revokeEveryone(String roleToRevoke);

    User save(User user);

    User saveOverrideRole(User user);

    void deleteByUsername(String username);

    /**
     * 按角色查找用户
     *
     * @param rolesList 角色列表
     * @return 用户列表
     */
    List<User> findUserByRoles(List<String> rolesList);

}
