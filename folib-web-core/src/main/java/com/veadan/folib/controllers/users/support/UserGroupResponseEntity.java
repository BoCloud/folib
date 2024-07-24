package com.veadan.folib.controllers.users.support;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.veadan.folib.dto.RoleResourceRefDTO;
import com.veadan.folib.dto.UserGroupDTO;
import lombok.Data;

import java.util.List;

/**
 * @author Steve Todorov
 */
@Data
@JsonInclude(JsonInclude.Include.NON_NULL)
public class UserGroupResponseEntity
{
    private UserGroupDTO userGroupDTO;

    private RoleResourceRefDTO roleResourceRefDTO;

}
