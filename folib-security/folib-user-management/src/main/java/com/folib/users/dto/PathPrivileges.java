package com.folib.users.dto;

import java.util.Set;

import com.folib.users.domain.Privileges;

public interface PathPrivileges
{

    String getPath();

    boolean isWildcard();

    Set<Privileges> getPrivileges();

}
