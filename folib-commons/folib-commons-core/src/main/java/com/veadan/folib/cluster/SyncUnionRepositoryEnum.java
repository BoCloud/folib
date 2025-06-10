package com.veadan.folib.cluster;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;

/**
 * @author leipenghui
 * 联邦仓库配置同步类型枚举
 */
@AllArgsConstructor
@NoArgsConstructor
@Getter
public enum SyncUnionRepositoryEnum {

    /**
     * ADD_OR_UPDATE
     */
    ADD_OR_UPDATE(1),
    ;

    private Integer type;
}
