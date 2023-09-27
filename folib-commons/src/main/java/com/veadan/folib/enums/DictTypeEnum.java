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
    ;

    private String type;

}
