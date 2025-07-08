package com.folib.converters;

import com.folib.authorization.dto.RoleDto;
import com.folib.domain.RoleModel;
import com.folib.users.dto.AccessModelDto;

/**
 * @author Veadan
 */
public class RoleModelToRoleConverter {

    public static RoleDto convert(RoleModel roleModel) {
        AccessModelDto accessModelDto = AccessModelToUserAccessModelDtoConverter.convert(roleModel.getAccessModel());
        RoleDto role = new RoleDto(roleModel.getName(), roleModel.getDescription(), accessModelDto);
        return role;
    }

}
