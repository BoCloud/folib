package com.veadan.folib.storage.repository;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import javax.validation.Valid;
import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.NotNull;
import java.util.List;

/**
 * @author veadan
 */
@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class RepositoryPermissionDto {

    /**
     * 仓库可见范围 1 存储空间内 2 公开
     */
    @NotNull(message = "A scope must be specified.")
    private Integer scope;

    /**
     * 是否允许匿名访问
     */
    @NotNull(message = "A allowAnonymous must be specified.")
    private boolean allowAnonymous;

    /**
     * 仓库权限定义
     */
    @Valid
    private List<RepositoryPermissionUserDto> userList;

}
