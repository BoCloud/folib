package com.folib.domain;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.Set;

/**
 * @author veadan
 * @date 2024/01/24
 **/
@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class UserRepositoryPermission {
    /**
     * 存储空间管理员
     */
    private String storageAdmin;
    /**
     * 权限信息
     */
    private Set<String> permissions;
}
