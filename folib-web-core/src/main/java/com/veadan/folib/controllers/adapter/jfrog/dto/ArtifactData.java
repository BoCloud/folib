package com.veadan.folib.controllers.adapter.jfrog.dto;

import com.alibaba.fastjson.annotation.JSONField;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * @author leipenghui
 * @date 2024/2/26
 **/
@Builder
@Data
@AllArgsConstructor
@NoArgsConstructor
public class ArtifactData {

    /**
     * 名称
     */
    private String name;
    /**
     * 镜像名称
     */
    @JSONField(name = "image_name")
    private String imageName;
    /**
     * tag
     */
    private String tag;
    /**
     * 路径
     */
    private String path;
    /**
     * 仓库
     */
    @JSONField(name = "repo_key")
    private String repoKey;
    /**
     * sha256
     */
    private String sha256;
    /**
     * 大小
     */
    private long size;
    /**
     * 源路径
     */
    @JSONField(name = "source_repo_path")
    private String sourceRepoPath;
    /**
     * 目标路径
     */
    @JSONField(name = "target_repo_path")
    private String targetRepoPath;
}
