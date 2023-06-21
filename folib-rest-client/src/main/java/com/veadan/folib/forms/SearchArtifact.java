package com.veadan.folib.forms;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * @author leipenghui
 * @date 2022/11/23
 **/
@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class SearchArtifact {

    /**
     * 制品名称 必填
     */
    private String artifactName;

    /**
     * 元数据搜索（二者默认其中一个必填）
     */
    private String metadataSearch;

    /**
     * 存储空间id
     */
    private String storageId;
    /**
     * 仓库id
     */
    private String repositoryId;
    /**
     * 仓库id集合，逗号分隔
     */
    private String repositoryIds;
    /**
     * 开启正则匹配 true 开启 false 不开启
     */
    private Boolean regex;
    /**
     * 自定义的正则表达式
     */
    private String pattern;
    /**
     * 排序字段
     */
    private String sortField;
    /**
     * 顺序
     */
    private String sortOrder;
    /**
     * 每页数据
     */
    private Integer limit;
    /**
     * 页码
     */
    private Integer page;
}
