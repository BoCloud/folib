package com.veadan.folib.ws.server;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Builder
@Data
@AllArgsConstructor
@NoArgsConstructor
@Deprecated
public class TargetTaskQueueV2Manager {

    private String targetHostName;

    private TaskQueueV2Manager taskQueueV2Manager;
}
