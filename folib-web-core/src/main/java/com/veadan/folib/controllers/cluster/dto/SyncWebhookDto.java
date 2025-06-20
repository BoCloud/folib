package com.veadan.folib.controllers.cluster.dto;

import com.veadan.folib.cluster.SyncWebhookEnum;
import com.veadan.folib.dto.configuration.WebhookConfigurationDto;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * @author leipenghui
 */
@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class SyncWebhookDto {

    private WebhookConfigurationDto webhookConfigurationForm;

    private SyncWebhookEnum syncWebhookEnum;
}
