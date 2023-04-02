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
    VULNERABILITY_DATA_UPDATE("vulnerability_data_update"),
    ;

    private String type;

}
