package com.veadan.folib.domain;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;

/**
 * @author veadan
 * @date 2023/10/11
 **/
@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
public class ArtifactSearchCondition {

    /**
     * 存储空间
     */
    private String storageId;
    /**
     * 仓库
     */
    private String repositoryId;
    /**
     * 路径前缀
     */
    private String path;
    /**
     * 查询类型
     */
    private String type;
    /**
     * 条件组集合
     */
    private List<ArtifactConditionGroup> artifactConditionGroups;
    /**
     * 排序
     */
    private List<ArtifactSort> artifactSorts;
}
