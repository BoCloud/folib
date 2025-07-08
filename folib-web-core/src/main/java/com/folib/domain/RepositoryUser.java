package com.folib.domain;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;

/**
 * @author veadan
 * @date 2023/3/7
 **/
@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
public class RepositoryUser {

    /**
     * 用户名
     */
    private String username;

    /**
     * 权限
     */
    private List<String> permissions;

    /**
     * 路径
     */
    private String paths;
}
