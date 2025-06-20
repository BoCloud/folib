package com.veadan.folib.converters;

import com.veadan.folib.converters.users.AccessModelFormToUserAccessModelDtoConverter;
import com.veadan.folib.dto.RoleDto;
import com.veadan.folib.users.dto.AccessModelDto;
import org.springframework.core.convert.converter.Converter;

/**
 * @author Veadan
 */
public class RoleFormToRoleConverter
        implements Converter<RoleDto, com.veadan.folib.authorization.dto.RoleDto>
{

    public static final RoleFormToRoleConverter INSTANCE = new RoleFormToRoleConverter();

    @Override
    public com.veadan.folib.authorization.dto.RoleDto convert(RoleDto roleForm)
    {
        AccessModelDto accessModelDto = AccessModelFormToUserAccessModelDtoConverter.INSTANCE.convert(roleForm.getAccessModel());
        com.veadan.folib.authorization.dto.RoleDto role = new com.veadan.folib.authorization.dto.RoleDto(roleForm.getName(), roleForm.getDescription(), accessModelDto);

        return role;
    }
    
}
