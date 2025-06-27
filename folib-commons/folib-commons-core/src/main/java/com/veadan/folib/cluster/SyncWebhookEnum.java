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
public enum SyncWebhookEnum {

    /**
     * ADD
     */
    ADD(1),
    /**
     * UPDATE
     */
    UPDATE(2),
    /**
     * DELETE
     */
    DELETE(3);

    private Integer type;
}
