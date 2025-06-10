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
public class ArtifactCondition {

    /**
     * 条件类型
     */
    private ArtifactSearchConditionTypeEnum artifactSearchConditionTypeEnum;
    /**
     * 搜索key
     */
    private String searchKey;
    /**
     * 搜索值
     */
    private String searchValue;
    /**
     * 搜索值后缀结束
     */
    private Boolean searchValueSuffixEnd;
}
