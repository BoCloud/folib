package com.veadan.folib.authorization.service;

import com.veadan.folib.authorization.domain.AuthorizationConfig;
import com.veadan.folib.authorization.dto.RoleDto;
import com.veadan.folib.users.domain.Privileges;
import com.veadan.folib.authorization.dto.AuthorizationConfigDto;

import java.io.IOException;
import java.util.List;

/**
 * @author 
 * @author veadan
 */
public interface AuthorizationConfigService
{

    void setAuthorizationConfig(AuthorizationConfigDto config) throws IOException;

    AuthorizationConfigDto getDto();

    AuthorizationConfig get();

    void addRole(RoleDto role) throws IOException;

    boolean deleteRole(String roleName) throws IOException;

    void addPrivilegesToAnonymous(List<Privileges> privilegeList) throws IOException;

    /**
     * 处理角色信息
     *
     * @param roleInfo 角色信息
     */
    void handlerRole(String roleInfo);

    void clearPrivilegesAnonymous() throws IOException;
}
