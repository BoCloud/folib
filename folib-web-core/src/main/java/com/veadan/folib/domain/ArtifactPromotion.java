package com.veadan.folib.domain;

import com.veadan.folib.dto.TargetRepositoyDto;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import javax.validation.constraints.NotEmpty;
import java.util.List;

/**
 * 制品晋级参数
 *
 * @author qijianping
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
