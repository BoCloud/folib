package com.veadan.folib.converters;

import com.veadan.folib.authorization.dto.RoleDto;
import com.veadan.folib.domain.RoleModel;
import com.veadan.folib.users.dto.AccessModelDto;

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
