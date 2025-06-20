package com.veadan.folib.dto.component;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;

/**
 * 组件图谱vo
 *
 * @author leipenghui
 **/
@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class ArtifactStatisticsDto implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * 影响存储空间数量
     */
    private Integer storageCount;

    /**
     * 影响仓库数量
     */
    private Integer repositoryCount;

    /**
     * 影响制品数量
     */
    private Integer artifactCount;
}
