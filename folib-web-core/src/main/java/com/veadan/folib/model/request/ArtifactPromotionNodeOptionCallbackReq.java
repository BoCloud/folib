package com.veadan.folib.model.request;

import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

/**
 * @author xiaodong.wang
 * @email wangxiaodong@beyondcent.com
 * @date 2023/11/23 15:25
 * @since x.x.x
 */
@Data
@Accessors(chain = true)
public class ArtifactPromotionNodeOptionCallbackReq {
    @ApiModelProperty("制品同步编号")
    private String syncNo;
    /**
     * 同步状态（1：就绪；2：同步中；3：成功；4：失败）
     * {@linkplain com.veadan.folib.enums.ArtifactSyncRecordStatusEnum }
     */
    @ApiModelProperty("同步状态（1：就绪；2：同步中；3：成功；4：失败）")
    private Integer status;
    /**
     * 失败的原因
     */
    @ApiModelProperty("失败的原因")
    private String failedReason;
}
