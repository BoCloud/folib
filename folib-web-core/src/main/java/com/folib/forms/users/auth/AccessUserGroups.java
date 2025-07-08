package com.folib.forms.users.auth;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.apache.commons.lang3.StringUtils;

import javax.validation.constraints.AssertTrue;
import java.util.ArrayList;
import java.util.List;

/**
 * @Author: fengmg
 * @Date: 2024/8/2 10:24
 * @Description:
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class AccessUserGroups {
    /**用户组id*/
//    @NotBlank(groups = {RoleForm.NewRole.class, RoleForm.UpdateRole.class}, message = "A name id be specified.")
    private String id;

    /**用户组名称*/
//    @NotBlank(groups = {RoleForm.NewRole.class, RoleForm.UpdateRole.class}, message = "A name be specified.")
    private String name;

    private List<String> access = new ArrayList<>();

    @AssertTrue(groups = {RoleForm.NewRole.class, RoleForm.UpdateRole.class}, message = "id or name is required.")
    private boolean isIdOrNameExists() {
        return StringUtils.isNotBlank(id) || StringUtils.isNotBlank(name);
    }
}
