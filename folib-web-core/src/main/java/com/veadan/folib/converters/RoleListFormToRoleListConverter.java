package com.veadan.folib.converters;

import com.veadan.folib.dto.RoleDto;
import com.veadan.folib.dto.RoleListDto;

import java.util.ArrayList;
import java.util.List;

import org.springframework.core.convert.converter.Converter;

/**
 * @author Veadan
 */
public class RoleListFormToRoleListConverter
        implements Converter<RoleListDto, List<com.veadan.folib.authorization.dto.RoleDto>>
{
    
    @Override
    public List<com.veadan.folib.authorization.dto.RoleDto> convert(RoleListDto roleListForm)
    {
        List<com.veadan.folib.authorization.dto.RoleDto> roleList = new ArrayList<>();
        for (RoleDto roleForm : roleListForm.getRoles())
        {
            com.veadan.folib.authorization.dto.RoleDto role = RoleFormToRoleConverter.INSTANCE.convert(roleForm);
            roleList.add(role);
        }

        return roleList;
    }
    
}
