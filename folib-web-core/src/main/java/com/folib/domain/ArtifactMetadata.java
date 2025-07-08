package com.folib.domain;

import com.alibaba.fastjson.annotation.JSONField;
import com.folib.enums.ArtifactMetadataEnum;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * @author veadan
 * @date 2022/11/30
 **/
@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class ArtifactMetadata {

    /**
     * 元数据值
     */
    @JSONField(ordinal = 1)
    private String value;
    /**
     * 元数据类型
     *
     * @see ArtifactMetadataEnum
     */
    @JSONField(ordinal = 2)
    private String type;
    /**
     * 前端是否展示
     */
    @JSONField(ordinal = 3)
    private Integer viewShow;
    /**
     * 展示位置
     */
    @JSONField(ordinal = 4)
    private String location;
}
