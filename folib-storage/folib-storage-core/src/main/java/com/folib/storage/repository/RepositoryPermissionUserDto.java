package com.folib.storage.repository;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotEmpty;
import java.util.List;

/**
 * @author veadan
 */
@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class RepositoryPermissionUserDto {

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

    /**
     * 路径
     */
    private List<String> paths;
}
