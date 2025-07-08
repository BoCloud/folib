package com.folib.model.request;

import com.folib.constant.ArtifactSyncRecordStatusEnum;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

/**
 * @author veadan
 * @date 2023/11/23 15:25
 */
@Data
@Accessors(chain = true)
public class ArtifactPromotionNodeOptionCallbackReq {
    @ApiModelProperty("制品同步编号")
    private String syncNo;
    /**
     * 同步状态（1：就绪；2：同步中；3：成功；4：失败）
     * {@linkplain ArtifactSyncRecordStatusEnum }
     */
    @ApiModelProperty("同步状态（1：就绪；2：同步中；3：成功；4：失败）")
    private Integer status;
    /**
     * 失败的原因
     */
    @ApiModelProperty("失败的原因")
    private String failedReason;
}
