package com.folib.converters;

import com.folib.authorization.dto.RoleDto;
import com.folib.converters.users.AccessModelFormToUserAccessModelDtoConverter;
import com.folib.forms.RoleForm;
import com.folib.users.dto.AccessModelDto;
import org.springframework.core.convert.converter.Converter;

/**
 * @author Veadan
 */
public class RoleFormToRoleConverter
        implements Converter<RoleForm, RoleDto>
{

    public static final RoleFormToRoleConverter INSTANCE = new RoleFormToRoleConverter();

    @Override
    public  RoleDto convert(RoleForm roleForm)
    {
        AccessModelDto accessModelDto = AccessModelFormToUserAccessModelDtoConverter.INSTANCE.convert(roleForm.getAccessModel());
        RoleDto role = new RoleDto(roleForm.getName(), roleForm.getDescription(), accessModelDto);

        return role;
    }
    
}
