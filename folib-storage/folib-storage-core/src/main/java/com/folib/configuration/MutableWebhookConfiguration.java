package com.folib.configuration;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;
import java.util.Objects;
import java.util.Set;

/**
 * @author veadan
 */
@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class MutableWebhookConfiguration
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

    //事件仓库 storageId:repositoryId
    private Set<String>  repository;

    /**
     * 触发事件
     */
    private Set<String> events;
    /**
     * 启用ssl true 启用 false 不启用
     */
    private Boolean ssl;

    @Override
    public boolean equals(Object o) {
        if (this == o) {
            return true;
        }
        if (!(o instanceof MutableWebhookConfiguration)) {
            return false;
        }
        MutableWebhookConfiguration that = (MutableWebhookConfiguration) o;
        return uuid.equals(that.uuid);
    }

    @Override
    public int hashCode() {
        return Objects.hash(uuid);
    }
}
