package com.veadan.folib.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * 制品分发仓库参数
 *
 * @author veadan
 */
@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class ArtifactDispatchRepositoryDto {
    /**
     * 节点英文名（唯一）
     */
    private String dispatchEnName;

    /**
     * 分发类型 pull  push
     */
    private String type;

    /**
     * 布局类型
     */
    private String layout;

    /**
     * 策略
     */
    private String policy;
}
