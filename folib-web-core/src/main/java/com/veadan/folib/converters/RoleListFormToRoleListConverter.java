package com.veadan.folib.converters;



import java.util.ArrayList;
import java.util.List;

import com.veadan.folib.authorization.dto.RoleDto;
import com.veadan.folib.forms.RoleForm;
import com.veadan.folib.forms.RoleListForm;
import org.springframework.core.convert.converter.Converter;

/**
 * @author Veadan
 */
public class RoleListFormToRoleListConverter
        implements Converter<RoleListForm, List<RoleDto>>
{
    
    @Override
    public List<com.veadan.folib.authorization.dto.RoleDto> convert(RoleListForm roleListForm)
    {
        List<com.veadan.folib.authorization.dto.RoleDto> roleList = new ArrayList<>();
        for (RoleForm roleForm : roleListForm.getRoles())
        {
            com.veadan.folib.authorization.dto.RoleDto role = RoleFormToRoleConverter.INSTANCE.convert(roleForm);
            roleList.add(role);
        }

        return roleList;
    }
    
}
