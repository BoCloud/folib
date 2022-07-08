package com.veadan.folib.users.service;

import com.veadan.folib.domain.User;
import com.veadan.folib.users.domain.Users;
import org.jose4j.lang.JoseException;

/**
 * @author Alex Oreshkevich
 * @author veadan
 */
public interface UserService
{

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
     * This method is mainly necessary for the UI - for users to be able to update their own account data
     * (i.e. change password or securityToken)
     *
     * @param userToUpdate
     */
    void updateAccountDetailsByUsername(User userToUpdate);

    Users getUsers();

    void revokeEveryone(String roleToRevoke);

    User save(User user);

    void deleteByUsername(String username);

}
