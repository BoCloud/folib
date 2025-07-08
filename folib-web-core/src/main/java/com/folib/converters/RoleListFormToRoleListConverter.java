package com.folib.converters;



import java.util.ArrayList;
import java.util.List;

import com.folib.authorization.dto.RoleDto;
import com.folib.forms.RoleForm;
import com.folib.forms.RoleListForm;
import org.springframework.core.convert.converter.Converter;

/**
 * @author Veadan
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
