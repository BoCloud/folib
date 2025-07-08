package com.folib.users.service;

import com.folib.domain.SecurityRole;

/**
 * @author veadan
 */
public interface SecurityRoleService
{

    SecurityRole findOneOrCreate(String role);

}
