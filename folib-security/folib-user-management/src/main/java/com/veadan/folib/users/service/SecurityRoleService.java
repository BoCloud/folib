package com.veadan.folib.users.service;

import com.veadan.folib.domain.SecurityRole;

/**
 * @author ankit.tomar
 */
public interface SecurityRoleService
{

    SecurityRole findOneOrCreate(String role);

}
