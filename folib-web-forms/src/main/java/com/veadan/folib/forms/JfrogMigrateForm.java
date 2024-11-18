package com.veadan.folib.forms;

import lombok.Data;

import javax.validation.constraints.NotBlank;
import javax.validation.constraints.Pattern;
import java.util.Set;

/**
 * @author huayanjun
 * @since 2024-10-22 17:10
 */

@Data
public class JfrogMigrateForm {

    @NotBlank(message = "用户名不能为空")
    private String username;

    @NotBlank(message = "密码不能为空")
    private String password;

    @NotBlank(message = "jfrog地址不能为空")
    private String url;

    // USER GROUP PERMISSION REPOSITORY
    private Set<String> contents;

    @Pattern(regexp = "[a-zA-Z0-9\\-\\_\\.]+")
    private String storageId;

    private String storageProvider;

    private String basedir;
}
