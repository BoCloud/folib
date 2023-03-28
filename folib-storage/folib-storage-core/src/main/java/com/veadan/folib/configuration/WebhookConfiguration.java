package com.veadan.folib.configuration;

import com.beust.jcommander.internal.Sets;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import javax.annotation.concurrent.Immutable;
import java.io.Serializable;
import java.util.Objects;
import java.util.Set;

/**
 * @author leipenghui
 */
@Immutable
@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class WebhookConfiguration
        implements Serializable {

    /**
     * uuid
     */
    private String uuid;

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

    public WebhookConfiguration(MutableWebhookConfiguration mutableWebhookConfiguration) {
        this.uuid = mutableWebhookConfiguration.getUuid();
        this.url = mutableWebhookConfiguration.getUrl();
        this.accessToken = mutableWebhookConfiguration.getAccessToken();
        this.events = mutableWebhookConfiguration.getEvents();
        this.ssl = mutableWebhookConfiguration.getSsl();
    }

    public Set<String> getEvents() {
        return Objects.isNull(events) ? Sets.newLinkedHashSet() : events;
    }

}
