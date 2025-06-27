package com.veadan.folib.cluster;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;

/**
 * @author veadan
 * 授权配置同步类型枚举
 */
@AllArgsConstructor
@NoArgsConstructor
@Getter
public enum SyncAuthorizationEnum {
    /**
     * 更新
     */
    UPDATE(1),
    ;

    private Integer type;
    }
