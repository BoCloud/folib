package com.veadan.folib.vo;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.Set;

/**
 * @author leipenghui
 * @date 2022/11/14
 **/
@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class Repository {

    /**
     * 仓库名称
     */
    private String id;

    /**
     * 仓库路径
     */
    private String basedir;

    /**
     * 版本策略
     */
    private String policy;

    /**
     * 文件存储方式
     */
    private String storageProvider;

    /**
     * 布局
     */
    private String layout;

    /**
     * 仓库类型
     */
    private String type;

    /**
     * 制品大小限制(MB)
     */
    private long artifactMaxSize;

    /**
     * 回收站
     */
    private boolean trashEnabled;

    /**
     * 删除
     */
    private boolean allowsDeletion;

    /**
     * 强制删除
     */
    private boolean allowsForceDeletion;

    /**
     * 上传部署
     */
    private boolean allowsDeployment;

    /**
     * 上传覆盖
     */
    private boolean allowsRedeployment;

    /**
     * 目录浏览
     */
    private boolean allowsDirectoryBrowsing;

    /**
     * 白名单列表
     */
    private Set<String> vulnerabilityWhites;

    /**
     * 黑名单列表
     */
    private Set<String> vulnerabilityBlacks;
}
