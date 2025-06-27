package com.veadan.folib.domain;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;

/**
 * @author veadan
 * @date 2023/3/8
 **/
@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
public class RepositoryPermission {

    /**
     * 仓库可见范围 1 存储空间内 2 公开
     */
    private Integer scope;
    /**
     * 是否允许匿名访问
     */
    private boolean allowAnonymous;
    /**
     * 权限
     */
    private List<RepositoryUser> userList;
}
