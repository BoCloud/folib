package com.veadan.folib.enums;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;

/**
 * @author huayanjun
 * @since 2024-12-27 09:44
 */
@AllArgsConstructor
@NoArgsConstructor
@Getter
public enum MigrateStatusEnum {

    INITIAL(0, "初始"),
    QUEUING(1, "排队"),
    FETCHING_INDEX(2, "获取索引"),
    SYNCING_ARTIFACT(3, "同步制品"),
    PAUSED(4, "暂停"),
    COMPLETED(5, "完成"),
    INDEX_FAILED(6, "同步索引失败"),
    SYNCING_FAILED(7,"同步制品失败"),
    END(8, "结束");


    private int status;

    private String message;





}
