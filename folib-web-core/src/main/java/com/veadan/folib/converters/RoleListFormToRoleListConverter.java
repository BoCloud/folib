package com.veadan.folib.converters;

import com.veadan.folib.forms.RoleForm;
import com.veadan.folib.forms.RoleListForm;
import com.veadan.folib.authorization.dto.RoleDto;

import java.util.ArrayList;
import java.util.List;

import org.springframework.core.convert.converter.Converter;

/**
 * @author Pablo Tirado
 */
public class RoleListFormToRoleListConverter
        implements Converter<RoleListForm, List<RoleDto>>
{
    
    @Override
    public List<RoleDto> convert(RoleListForm roleListForm)
    {
        List<RoleDto> roleList = new ArrayList<>();
        for (RoleForm roleForm : roleListForm.getRoles())
        {
            RoleDto role = RoleFormToRoleConverter.INSTANCE.convert(roleForm);
            roleList.add(role);
        }

        return roleList;
    }
    
}
