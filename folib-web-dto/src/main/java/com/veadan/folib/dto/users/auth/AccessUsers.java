package com.veadan.folib.dto.users.auth;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import javax.validation.constraints.NotBlank;
import java.util.ArrayList;
import java.util.List;

/**
 * @author veadan
 * @Date: 2024/8/2 10:24
 * @Description:
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class AccessUsers {

    /**用户id*/
    @NotBlank(groups = {RoleDto.NewRole.class, RoleDto.UpdateRole.class}, message = "A name id be specified.")
    private String id;

    private List<String> access = new ArrayList<>();
}
