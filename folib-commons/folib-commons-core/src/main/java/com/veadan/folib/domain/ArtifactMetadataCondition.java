package com.veadan.folib.domain;

import com.veadan.folib.enums.ArtifactSearchConditionTypeEnum;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * @author leipenghui
 * @date 2023/10/11
 **/
@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
public class ArtifactMetadataCondition {

    /**
     * 条件类型
     */
    private ArtifactSearchConditionTypeEnum artifactSearchConditionTypeEnum;

    /**
     * 元数据key
     */
    private String medataKey;

    /**
     * 元数据Value
     */
    private String medataValue;
}
