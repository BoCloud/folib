package com.folib.forms.configuration;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.folib.configuration.MutableWebhookConfiguration;
import com.folib.configuration.WebhookConfiguration;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotEmpty;
import java.io.Serializable;
import java.util.Optional;
import java.util.Set;

/**
 * @author leipenghui
 */
@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class WebhookConfigurationForm
        implements Serializable {

    /**
     * uuid
     */
    @NotBlank(message = "uuid不能为空", groups = {UpdateGroup.class, DeleteGroup.class, TestGroup.class})
    private String uuid;
    /**
     * url
     */
    @NotBlank(message = "请填写url", groups = {AddGroup.class})
    private String url;

    /**
     * 访问令牌
     */
    private String accessToken;

    /**
     * 触发事件
     */
    @NotEmpty(message = "请选择触发事件", groups = {AddGroup.class, TestGroup.class})
    private Set<String> events;
    /**
     * 启用ssl true 启用 false 不启用
     */
    private Boolean ssl;

    /**
     * 所属仓库 不填则对所有仓库生效
     */
    private Set<String>  repository;

    public WebhookConfigurationForm(WebhookConfiguration webhookConfiguration) {
        this.uuid = webhookConfiguration.getUuid();
        this.url = webhookConfiguration.getUrl();
        this.accessToken = webhookConfiguration.getAccessToken();
        this.events = webhookConfiguration.getEvents();
        this.ssl = webhookConfiguration.getSsl();
        this.repository= webhookConfiguration.getRepository();
    }

    @JsonIgnore()
    public static WebhookConfigurationForm fromConfiguration(WebhookConfiguration source) {
        WebhookConfiguration webhookConfiguration = Optional.ofNullable(source).orElse(
                new WebhookConfiguration(new MutableWebhookConfiguration())
        );
        return new WebhookConfigurationForm(webhookConfiguration);
    }

    public interface AddGroup
            extends Serializable {
    }

    public interface UpdateGroup
            extends Serializable {
    }

    public interface DeleteGroup
            extends Serializable {
    }

    public interface TestGroup
            extends Serializable {
    }
}