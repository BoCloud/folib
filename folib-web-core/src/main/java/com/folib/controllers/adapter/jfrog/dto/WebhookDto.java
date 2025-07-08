package com.folib.controllers.adapter.jfrog.dto;

import com.alibaba.fastjson.annotation.JSONField;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * @author veadan
 * @date 2024/2/26
 **/
@Builder
@Data
@AllArgsConstructor
@NoArgsConstructor
public class WebhookDto {

    /**
     * 事件类型
     */
    @JSONField(name = "event_type")
    private String eventType;

    /**
     * 制品数据
     */
    private ArtifactData data;

    /**
     * 域
     */
    private String domain;
}
