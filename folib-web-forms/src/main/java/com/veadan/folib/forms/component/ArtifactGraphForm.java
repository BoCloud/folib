package com.veadan.folib.forms.component;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;
import java.util.List;

/**
 * 组件图谱vo
 *
 * @author leipenghui
 **/
@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class ArtifactGraphForm implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * 组件uuid、存储空间id、仓库id、制品uuid
     */
    private String id;
    /**
     * 标题
     */
    private String title;
    /**
     * 名称
     */
    private String name;
    /**
     * 版本
     */
    private String version;
    /**
     * 统计数
     */
    private Integer count;
    /**
     * 底色
     */
    private String color;
    /**
     * 方向
     */
    private Boolean up;
    /**
     * 子节点
     */
    private List<ArtifactGraphForm> children;
}
