package com.veadan.folib.domain;

import io.swagger.annotations.ApiModelProperty;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import javax.persistence.Column;
import javax.validation.constraints.NotEmpty;

/**
 * 节点之间的晋级
 *
 * @author qijianping
 */
@AllArgsConstructor
@NoArgsConstructor
@Data
public class PromotionNodeOption {

    @NotEmpty
    private String sourcePath;

    @NotEmpty
    private String targetPath;

    /**
     * 同步模式（1：推；2：拉）
     * {@linkplain  com.veadan.folib.enums.ArtifactSyncRecordSyncModelEnum }
     */
    @ApiModelProperty("同步模式（1：推；2：拉）")
    private Integer syncModel;

    /**
     * 制品同步编号，无需调用方提供
     */
    private String syncNo;

    /**
     * WS通道目标节点，无需调用方提供
     */
    private String targetNode;

    /**
     * 是否重试
     */
    private boolean isRetry;

    public PromotionNodeOption(String sourcePath, String targetPath) {
        this.sourcePath = sourcePath;
        this.targetPath = targetPath;
    }
}
