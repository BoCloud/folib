package com.veadan.folib.model.response;

import cn.hutool.core.bean.BeanUtil;
import com.fasterxml.jackson.annotation.JsonFormat;
import com.veadan.folib.constant.ArtifactSyncRecordStatusEnum;
import com.veadan.folib.entity.ArtifactSyncRecord;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

import java.util.Date;

/**
 * @author xiaodong.wang
 * @email wangxiaodong@beyondcent.com
 * @date 2023/12/6 14:12
 * @since x.x.x
 */
@Data
@ApiModel("制品晋级/分发记录分页-响应模型")
public class ArtifactSyncRecordPageRes {
    @ApiModelProperty("id")
    private Long id;

    /**
     * 请求主机名称
     */
    @ApiModelProperty("请求主机名称")
    private String requestHostName;

    @ApiModelProperty("源存储空间")
    private String sourceStorageId;

    @ApiModelProperty("源源仓库")
    private String sourceRepositoryId;

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
    @ApiModelProperty("从记录已被清除")
    private Boolean slaveRecordCleared;
    @ApiModelProperty("同步进度")
    private Double syncProgress;
    
    public ArtifactSyncRecordPageRes(ArtifactSyncRecord original) {
        if (null != original) {
            BeanUtil.copyProperties(original, this);
        }
    }
}
