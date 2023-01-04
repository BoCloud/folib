package com.veadan.folib.forms;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class ArtifactMetadataForm {
    /**
     * 元数据类型 [NUMERICAL, STRING,TEXT, MD,JSON]（数字，字符串，文本，Markdown，JSON）
     */
    private String type;

    /**
     * 前端是否展示 (前端是否展示仅为0或1)
     */
    private Integer viewShow;

    /**
     * 元数据key
     */
    private String key;

    /**
     * 元数据值
     */
    private String value;

    /**
     * 存储空间名称
     */
    private String storageId;

    /***
     * 仓库名称
     */
    private String repositoryId;

    /**
     * 制品路径
     */
    private String artifactPath;
}
