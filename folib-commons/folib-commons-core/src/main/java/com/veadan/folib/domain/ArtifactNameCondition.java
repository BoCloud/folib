package com.veadan.folib.domain;

import com.veadan.folib.enums.ArtifactSearchConditionTypeEnum;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * @author veadan
 * @date 2023/10/11
 **/
@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
public class ArtifactNameCondition {

    /**
     * 条件类型
     */
    private ArtifactSearchConditionTypeEnum artifactSearchConditionTypeEnum;

    /**
     * 搜索值
     */
    private String searchValue;
}
