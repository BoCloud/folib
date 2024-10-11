package com.veadan.folib.enums;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;

/**
 * @Author: fengmg
 * @Date: 2024/10/10 22:04
 * @Description: 同步用户权限策略枚举
 */
@NoArgsConstructor
@AllArgsConstructor
@Getter
public enum SyncStrategyEnum {

    SOURCE_TO_TARGET("sourceToTarget", "源同步到目标"),
    TARGET_TO_SOURCE("targetToSource", "目标同步到源"),
    TWO_WAY_SYNC("twoWaySync", "双向同步");

    private String value;

    private String description;

}
