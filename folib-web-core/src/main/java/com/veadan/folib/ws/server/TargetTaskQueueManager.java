package com.veadan.folib.ws.server;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * @author leipenghui
 * @date 2024/4/12
 **/
@Builder
@Data
@AllArgsConstructor
@NoArgsConstructor
public class TargetTaskQueueManager {

    private String targetHostName;

    private TaskQueueManager taskQueueManager;
}
