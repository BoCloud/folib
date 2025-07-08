package com.folib.authorization.dto;

import com.folib.users.dto.AccessModel;

public interface Role
{

    String getName();

    String getDescription();

    AccessModel getAccessModel();

}
