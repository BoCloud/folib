package com.veadan.folib.controllers.users.support;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.veadan.folib.dto.RoleResourceRefDTO;
import com.veadan.folib.dto.UserGroupDTO;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

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
