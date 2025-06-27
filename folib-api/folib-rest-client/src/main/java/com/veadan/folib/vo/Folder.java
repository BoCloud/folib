package com.veadan.folib.vo;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.Date;
import java.util.List;

/**
 * @author veadan
 * @date 2022/11/16
 **/
@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class Folder {

    /**
     * 存储空间名称
     */
    private String storageId;
    /**
     * 仓库名称
     */
    private String repositoryId;
    /**
     * 名称
     */
    private String name;
    /**
     * 浏览路径
     */
    private String url;
    /**
     * 制品路径
     */
    private String artifactPath;
    /**
     * 最后修改时间
     */
    private Date lastModified;
    /**
     * 目录标识 true 是 false 不是
     */
    private boolean folder;
    /**
     * 文件大小
     */
    private Long size;
    /**
     * 子集
     */
    private List<Folder> children;
}
