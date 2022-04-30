package com.veadan.folib.users.dto;

import java.util.Set;

import com.veadan.folib.users.domain.Privileges;

public interface PathPrivileges
{

    String getPath();

    boolean isWildcard();

    Set<Privileges> getPrivileges();

}
