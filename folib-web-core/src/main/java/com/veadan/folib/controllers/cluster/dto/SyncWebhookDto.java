package com.veadan.folib.controllers.cluster.dto;

import com.veadan.folib.cluster.SyncWebhookEnum;
import com.veadan.folib.forms.configuration.WebhookConfigurationForm;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * @author veadan
 */
@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class SyncWebhookDto {

    private WebhookConfigurationForm webhookConfigurationForm;

    private SyncWebhookEnum syncWebhookEnum;
}
