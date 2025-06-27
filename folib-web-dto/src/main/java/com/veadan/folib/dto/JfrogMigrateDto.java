package com.veadan.folib.dto;

import lombok.Data;

import javax.validation.constraints.NotBlank;
import javax.validation.constraints.Pattern;
import java.util.Set;

/**
 * @author veadan
 * @since 2024-10-22 17:10
 */

@Data
public class JfrogMigrateDto {

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

    @NotBlank(message = "制品迁移方式不能为空1-jfrog备份 2-数据爬取")
    private String artifactType;
}
