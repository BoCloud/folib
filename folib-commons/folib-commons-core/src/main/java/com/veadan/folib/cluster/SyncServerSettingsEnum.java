package com.veadan.folib.cluster;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;

/**
 * @author leipenghui
 * 全局配置同步类型枚举
 */
@AllArgsConstructor
@NoArgsConstructor
@Getter
public enum SyncServerSettingsEnum {

    /**
     * ADD_OR_UPDATE
     */
    ADD_OR_UPDATE(1),
    ;

    private Integer type;
    }
