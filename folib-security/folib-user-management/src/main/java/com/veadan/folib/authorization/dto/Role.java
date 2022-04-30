package com.veadan.folib.authorization.dto;

import com.veadan.folib.users.dto.AccessModel;

public interface Role
{

    String getName();

    String getDescription();

    AccessModel getAccessModel();

}
