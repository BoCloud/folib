package com.folib.domain;

import com.folib.dto.TargetRepositoyDto;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import javax.validation.constraints.NotBlank;
import java.util.List;

/**
 * 制品晋级参数
 *
 * @author veadan
 */
@AllArgsConstructor
@NoArgsConstructor
@Data
public class ArtifactPromotion {
    /**
     * 源存储id
     */
    @NotBlank
    private String srcStorageId;

    /**
     * 源仓库id
     */
    @NotBlank
    private String srcRepositoryId;

    /**
     * 目标仓库集合
     */
    List<TargetRepositoyDto> targetRepositoyList;

    /**
     * 源制品uri
     */
    @NotBlank
    private String path;

    /**
     * 目标制品uri
     */
    private String targetPath;

}
