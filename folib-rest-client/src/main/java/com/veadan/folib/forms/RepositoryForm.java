package com.veadan.folib.forms;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * @author leipenghui
 * @date 2022/11/14
 **/
@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class RepositoryForm {

    /**
     * 仓库名称
     */
    private String id;

    /**
     * 路径
     */
    private String basedir;

    /**
     * 版本策略 默认 mixed
     */
    private String policy;

    /**
     * 文件存储方式 默认 local
     */
    private String storageProvider;

    /**
     * 布局类型
     */
    private String layout;

    /**
     * 仓库类型 默认 hosted
     */
    private String type;

    /**
     * 服务类型 In Service（开放） Out of Service（关闭）
     */
    private String status;

    /**
     * 单个制品最大值
     */
    @Builder.Default
    private long artifactMaxSize = 214748364800L;

    /**
     * 回收站
     */
    @Builder.Default
    private boolean trashEnabled = true;

    /**
     * 允许强制删除
     */
    @Builder.Default
    private boolean allowsForceDeletion = true;

    /**
     * 允许上传
     */
    @Builder.Default
    private boolean allowsDeployment = true;

    /**
     * 允许上传覆盖
     */
    @Builder.Default
    private boolean allowsRedeployment = true;

    /**
     * 允许删除
     */
    @Builder.Default
    private boolean allowsDeletion = true;

    /**
     * 允许目录浏览
     */
    @Builder.Default
    private boolean allowsDirectoryBrowsing = true;

    /**
     * 开启checksum
     */
    private boolean checksumHeadersEnabled;
    /**
     * 是否允许匿名访问
     */
    private boolean allowAnonymous;

}
