package com.veadan.folib.forms.configuration;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotEmpty;
import java.util.List;

/**
 * @author leipenghui
 */
@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class RepositoryPermissionUserForm {

    /**
     * 用户名
     */
    @NotBlank(message = "A username must be specified.")
    private String username;

    /**
     * 权限
     */
    @NotEmpty(message = "A permissions must be specified.")
    private List<String> permissions;

}
