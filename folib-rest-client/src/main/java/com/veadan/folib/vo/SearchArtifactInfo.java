package com.veadan.folib.vo;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;

/**
 * @author leipenghui
 * @date 2022/11/23
 **/
@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class SearchArtifactInfo {

    /**
     * 存储空间名称
     */
    private String storageId;
    /**
     * 仓库名称
     */
    private String repositoryId;
    /**
     * 制品路径
     */
    private String artifactPath;
    /**
     * 制品名称
     */
    private String artifactName;
    /**
     * 文件大小
     */
    private Long sizeInBytes;
    /**
     * 下载次数
     */
    private Integer downloadCount;
    /**
     * 创建时间
     */
    private String created;
    /**
     * 使用时间
     */
    private String lastUsed;
    /**
     * 修改时间
     */
    private String lastUpdated;
    /**
     * 布局类型
     */
    private String layout;
    /**
     * path
     */
    private String path;

    /**
     * app 应用包下载地址
     */
    private List<String> downloadFilesUrl;

    /**
     * 元数据
     */
    private String metadata;
}
