package com.veadan.folib.model.request;

import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

import javax.persistence.Column;
import java.util.Date;

/**
 * @author xiaodong.wang
 * @email wangxiaodong@beyondcent.com
 * @date 2023/12/20 14:06
 * @since x.x.x
 */
@Data
@Accessors(chain = true)
public class ArtifactSyncSlaveRecordUpdateReq {
    @ApiModelProperty("id")
    private Long id;
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
    /**
     * 更新人
     */
    @ApiModelProperty("更新人")
    private String updateBy;
    /**
     * 更新时间
     */
    @ApiModelProperty("更新时间")
    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    private Date updateTime;
}
