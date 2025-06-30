package com.veadan.folib.controllers.cluster.dto;

import com.veadan.folib.cluster.SyncServerSettingsEnum;
import com.veadan.folib.forms.configuration.ServerSettingsForm;
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
public class SyncServerSettingsDto {

    private ServerSettingsForm serverSettingsForm;

    private SyncServerSettingsEnum syncServerSettingsEnum;
}
