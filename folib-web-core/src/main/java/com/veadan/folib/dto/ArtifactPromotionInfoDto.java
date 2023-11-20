package com.veadan.folib.dto;

import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModelProperty;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import javax.persistence.Column;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import java.util.Date;

/**
 * @author xiaodong.wang
 * @email wangxiaodong@beyondcent.com
 * @date 2023/10/12 14:35
 * @since x.x.x
 */
@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class ArtifactPromotionInfoDto 
{
    @Id
    @GeneratedValue(generator = "JDBC",strategy = GenerationType.IDENTITY)
    @ApiModelProperty("id")
    private Long id;

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
     * 制品操作（1：制品晋级；2：制品分发）
     * {@linkplain com.veadan.folib.enums.ArtifactSyncRecordOpsTypeEnum }
     */
    @ApiModelProperty("制品操作（1：制品晋级；2：制品分发）")
    private Integer opsType;
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
     * 创建人
     */
    @ApiModelProperty("创建人")
    private String createdBy;
    /**
     * 创建时间
     */
    @ApiModelProperty("创建时间")
    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    private Date createdTime;
    /**
     * 更新人
     */
    @ApiModelProperty("更新人")
    private String updatedBy;
    /**
     * 更新时间
     */
    @ApiModelProperty("更新时间")
    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    private Date updatedTime;
}
