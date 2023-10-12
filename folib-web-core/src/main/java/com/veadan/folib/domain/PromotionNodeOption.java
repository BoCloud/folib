package com.veadan.folib.domain;

import io.swagger.annotations.ApiModelProperty;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

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
     * 制品操作（1：制品晋级；2：制品分发）
     * {@linkplain com.veadan.folib.enums.ArtifactSyncRecordOpsTypeEnum }
     */
    @ApiModelProperty("制品操作（1：制品晋级；2：制品分发）")
    private Integer opsType;
    /**
     * 同步模式（1：推；2：拉）
     * {@linkplain  com.veadan.folib.enums.ArtifactSyncRecordSyncModelEnum }
     */
    @ApiModelProperty("同步模式（1：推；2：拉）")
    private Integer syncModel;

    public PromotionNodeOption(String sourcePath, String targetPath) {
        this.sourcePath = sourcePath;
        this.targetPath = targetPath;
    }
}
