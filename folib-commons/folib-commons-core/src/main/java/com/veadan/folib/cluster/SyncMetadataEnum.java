package com.veadan.folib.cluster;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;

/**
 * @author leipenghui
 * 元数据配置同步类型枚举
 */
@AllArgsConstructor
@NoArgsConstructor
@Getter
public enum SyncMetadataEnum {
    /**
     * 新增或更新
     */
    ADD_OR_UPDATE(1),
    /**
     * 删除
     */
    DELETE(2);

    private Integer type;
}
