package com.veadan.folib.domain;

import com.veadan.folib.enums.ArtifactSyncRecordSyncModelEnum;
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
    @ApiModelProperty("同步模式（1：推；2：拉），缺省值：拉")
    private Integer syncModel = ArtifactSyncRecordSyncModelEnum.PULL.getVal();

    @ApiModelProperty("制品同步编号，无需调用方提供")
    private String syncNo;

    public PromotionNodeOption(String sourcePath, String targetPath) {
        this.sourcePath = sourcePath;
        this.targetPath = targetPath;
    }
}
