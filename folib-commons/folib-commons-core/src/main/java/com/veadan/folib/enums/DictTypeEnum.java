package com.veadan.folib.enums;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;

/**
 * @author leipenghui
 * @date 2023/03/01
 **/
@AllArgsConstructor
@NoArgsConstructor
@Getter
public enum DictTypeEnum {

    /**
     * 首次初始化
     */
    INITIAL_INITIALIZATION("initial_initialization"),
    /**
     * 上传进度
     */
    UPLOAD_PROCESS("upload_process"),
    /**
     * FOLIB升级任务
     */
    FOLIB_UPGRADE_TASK("folib_upgrade_task"),
    /**
     * 构建索引
     */
    BUILD_GRAPH_INDEX("build_graph_index"),
    /**
     * 漏洞数据更新
     */
    VULNERABILITY_UPDATE("vulnerability_update"),
    /**
     * 自动晋级阻断
     */
    PROMOTION_BLOCK("promotion_block"),
    /**
     * 系统参数
     */
    SYSTEM_PROPERTY("system_property"),
    /**
     * docker文件进度
     */
    DOCKER_RANGES("docker_ranges"),
    /**
     * docker数据
     */
    DOCKER_DATA("docker_data"),
    /**
     * 处理mavenIndexer
     */
    HANDLER_MAVEN_INDEXER("handler_maven_indexer"),
    /**
     * 备份策略
     */
    BACKUP_SETTINGS("backup_settings"),
    /**
     * 缓存策略
     */
    CACHE_SETTINGS("cache_settings"),
    /**
     * 页面上传制品大小限制
     */
    UI_UPLOAD_MAX_SIZE("ui_upload_max_size"),
    /**
     * 全量制品扫描
     */
    ARTIFACT_FULL_SCAN("artifact_full_scan"),
    /**
     * 集群节点
     */
    CLUSTER_NODES("cluster_nodes"),
    ;

    private String type;

}
