package com.veadan.folib.model.request;

import com.fasterxml.jackson.annotation.JsonFormat;
import com.veadan.folib.constant.ArtifactSyncRecordStatusEnum;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

import javax.validation.constraints.NotEmpty;
import java.util.Date;

/**
 * @author xiaodong.wang
 * @email wangxiaodong@beyondcent.com
 * @date 2023/12/20 14:06
 * @since x.x.x
 */
@Data
public class ArtifactSyncSlaveRecordAddReq {
    /**
     * 源制品路径
     */
    @ApiModelProperty("源制品路径")
    private String sourcePath;
    /**
     * 目标制品路径
     */
    @ApiModelProperty("目标制品路径")
    private String targetPath;
    /**
     * 制品同步编号
     */
    @ApiModelProperty("制品同步编号")
    private String syncNo;
    /**
     * 同步模式（1：推；2：拉）
     * {@linkplain  com.veadan.folib.enums.ArtifactSyncRecordSyncModelEnum }
     */
    @ApiModelProperty("同步模式（1：推；2：拉）")
    private Integer syncModel;
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
    /**
     * 创建人
     */
    @ApiModelProperty("创建人")
    private String createBy;
    /**
     * 创建时间
     */
    @ApiModelProperty("创建时间")
    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    private Date createTime;
    
    @ApiModelProperty("临时ID，用于返回给请求端")
    @NotEmpty(message = "临时ID不能为空")
    private String tempId;
}
