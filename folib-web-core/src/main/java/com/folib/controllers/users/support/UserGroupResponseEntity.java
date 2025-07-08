package com.folib.controllers.users.support;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.folib.dto.UserGroupDTO;
import lombok.Data;

import java.util.List;
import java.util.Map;

/**
 * @author Steve Todorov
 */
@Data
@JsonInclude(JsonInclude.Include.NON_NULL)
public class UserGroupResponseEntity
{
    private UserGroupDTO userGroupDTO;

    private Map<String, List<String>> roleAccess;

}
