package com.veadan.folib.domain;

import com.veadan.folib.dto.TargetDispatchRepositoryDto;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import javax.validation.constraints.NotEmpty;
import java.util.List;

/**
 * 制品分发实体
 *
 * @author qijianping
 */
@AllArgsConstructor
@NoArgsConstructor
@Data
public class ArtifactDispatch {
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
    List<TargetDispatchRepositoryDto> targetDispatchRepositoryList;

    /**
     * 制品uri
     */
    @NotEmpty
    private String path;

    /**
     * 仓库类型
     */
    private String type;

    /**
     * 布局
     */
    private String layout;

    /**
     * 策略
     */
    private String policy;
}
