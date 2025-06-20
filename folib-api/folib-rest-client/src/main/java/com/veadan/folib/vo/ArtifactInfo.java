package com.veadan.folib.vo;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;

/**
 * @author leipenghui
 * @date 2022/11/16
 **/
@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class ArtifactInfo {

    /**
     * 制品信息
     */
    private Artifact artifact;
    /**
     * docker镜像名称
     */
    private String imageName;
    /**
     * 文件大小
     */
    private Long size;
    /**
     * 下载次数
     */
    private Integer downloadCount;
    /**
     * 创建时间
     */
    private String createdTime;
    /**
     * 使用时间
     */
    private String lastUsedTime;
    /**
     * 修改时间
     */
    private String lastModified;
    /**
     * 目录结构树
     */
    private List<FileTree> listTree;
    /**
     * sha
     */
    private String sha;
    /**
     * md5
     */
    private String md5;
    /**
     * snippets
     */
    private List<CodeSnippet> snippets;

    //docker start
    /**
     * sha256
     */
    private String sha256;
    /**
     * manifest
     */
    private ImageManifest manifest;
    /**
     * manifestConfig
     */
    private ManifestConfig manifestConfig;
    //docker end

}
