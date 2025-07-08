package com.folib.forms.common;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;
import java.util.List;

/**
 * 树结构form
 *
 * @author leipenghui
 * @date 2022/11/21
 **/
@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class StorageTreeForm implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * id
     */
    private String id;
    /**
     * 名称
     */
    private String name;
    /**
     * 制品库类型
     */
    private String artifactoryRepositoryType;
    /**
     * key
     */
    private String key;
    /**
     * 类型
     */
    private String type;
    /**
     * 布局
     */
    private String layout;
    /**
     * 仓库可见范围
     */
    private Integer scope;
    /**
     * 子集
     */
    private List<StorageTreeForm> children;
}
