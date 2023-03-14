package com.veadan.folib.configuration;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;
import java.util.Set;

/**
 * @author leipenghui
 */
@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class MutableWebhookConfiguration
        implements Serializable {

    /**
     * url
     */
    private String url;

    /**
     * 访问令牌
     */
    private String accessToken;

    /**
     * 触发事件
     */
    private Set<String> events;
    /**
     * 启用ssl true 启用 false 不启用
     */
    private Boolean ssl;

}
