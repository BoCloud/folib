package com.folib.domain;

import com.folib.enums.ArtifactSearchConditionTypeEnum;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;

/**
 * @author veadan
 * @date 2023/12/25
 **/
@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
public class ArtifactConditionGroup {

    /**
     * 条件类型
     */
    private ArtifactSearchConditionTypeEnum artifactSearchConditionTypeEnum;
    /**
     * 搜索条件
     */
    private List<ArtifactCondition> artifactConditions;
    /**
     * 路径条件
     */
    private List<ArtifactNameCondition> artifactNameConditions;
    /**
     * 元数据条件
     */
    private List<ArtifactMetadataCondition> artifactMetadataConditions;

}
