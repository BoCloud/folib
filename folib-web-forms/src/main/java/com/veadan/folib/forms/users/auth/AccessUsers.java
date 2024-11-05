package com.veadan.folib.forms.users.auth;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import javax.validation.constraints.NotBlank;
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
public class AccessUsers {

    /**用户id*/
    @NotBlank(groups = {RoleForm.NewRole.class, RoleForm.UpdateRole.class}, message = "A name id be specified.")
    private String id;

    private List<String> access = new ArrayList<>();
}
