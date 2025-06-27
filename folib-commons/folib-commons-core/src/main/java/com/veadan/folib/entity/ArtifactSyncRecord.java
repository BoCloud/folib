package com.veadan.folib.entity;

import com.baomidou.mybatisplus.annotation.TableId;
import com.baomidou.mybatisplus.annotation.TableName;
import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import javax.persistence.Column;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import java.io.Serializable;
import java.util.Date;


/**
 * @author veadan
 * @date 2023/10/5 17:04
 */
@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
@Accessors(chain = true)
@TableName("artifact_sync_record")
@ApiModel("ArtifactSyncRecord")
public class ArtifactSyncRecord implements Serializable {
    private static final long serialVersionUID = 1L;

    @TableId
    @ApiModelProperty("id")
    @Column(name = "id")
    private Long id;

    /**
     * 请求主机名称
     */
    @ApiModelProperty("请求主机名称")
    @Column(name = "request_host_name")
    private String requestHostName;

    /**
     * 源制品存储空间
     */
    @ApiModelProperty("源制品存储空间")
    @Column(name = "source_storage_id")
    private String sourceStorageId;

    /**
     * 源制品仓库ID
     */
    @ApiModelProperty("源制品仓库ID")
    @Column(name = "source_repository_id")
    private String sourceRepositoryId;

    /**
     * 源制品路径
     */
    @ApiModelProperty("源制品路径")
    @Column(name = "source_path")
    private String sourcePath;
    /**
     * 目标制品路径
     */
    @ApiModelProperty("目标制品路径")
    @Column(name = "target_path")
    private String targetPath;
    /**
     * 制品操作（1：制品晋级；2：制品分发）
     *
     */
    @ApiModelProperty("制品操作（1：制品晋级；2：制品分发）")
    @Column(name = "ops_type")
    private Integer opsType;
    @ApiModelProperty("同步进度（只有被清除从数据采用持久化）")
    @Column(name = "sync_progress")
    private Double syncProgress;
    /**
     * 制品同步编号
     */
    @ApiModelProperty("制品同步编号")
    @Column(name = "sync_no")
    private String syncNo;
    /**
     * 同步模式（1：推；2：拉）
     *
     */
    @ApiModelProperty("同步模式（1：推；2：拉）")
    @Column(name = "sync_model")
    private Integer syncModel;
    /**
     * 同步状态（1：就绪；2：同步中；3：成功；4：失败）
     *
     */
    @ApiModelProperty("同步状态（1：就绪；2：同步中；3：成功；4：失败）")
    @Column(name = "status")
    private Integer status;
    /**
     * 失败的原因
     */
    @ApiModelProperty("失败的原因")
    @Column(name = "failed_reason")
    private String failedReason;
    /**
     * 是否重试
     */
    @ApiModelProperty("是否重试")
    @Column(name = "retry")
    private Integer retry;
    /**
     * 重试次数
     */
    @ApiModelProperty("重试次数")
    @Column(name = "retry_count")
    private Integer retryCount;
    /**
     * 重试时间
     */
    @ApiModelProperty("重试时间")
    @Column(name = "retry_time")
    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    private Date retryTime;
    /**
     * 创建人
     */
    @ApiModelProperty("创建人")
    @Column(name = "create_by")
    private String createBy;
    /**
     * 创建时间
     */
    @ApiModelProperty("创建时间")
    @Column(name = "create_time")
    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    private Date createTime;
    /**
     * 更新人
     */
    @ApiModelProperty("更新人")
    @Column(name = "update_by")
    private String updateBy;
    /**
     * 更新时间
     */
    @ApiModelProperty("更新时间")
    @Column(name = "update_time")
    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    private Date updateTime;
}
