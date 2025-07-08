package com.folib.dto;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import javax.validation.constraints.NotEmpty;
import java.util.List;

/**
 * @author veadan
 * @date 2022-11-18
 */
@AllArgsConstructor
@NoArgsConstructor
@Data
public class ArtifactPromotion {
    /**
     * 源存储id
     */
    @NotEmpty
    private String srcStorageId;

    /**
     * 源仓库id
     */
    @NotEmpty
    private String srcRepositoryId;

    /**
     * 目标仓库集合
     */
    List<TargetRepositoyDto> targetRepositoyList;

    /**
     * 制品uri
     */
    @NotEmpty
    private String path;

}
