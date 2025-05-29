package com.veadan.folib.entity;

import com.baomidou.mybatisplus.annotation.TableField;
import com.baomidou.mybatisplus.annotation.TableId;
import com.baomidou.mybatisplus.annotation.TableName;
import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import javax.persistence.Column;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.Table;
import javax.persistence.Transient;
import java.io.Serializable;
import java.util.Date;


/**
 * 制品同步从记录
 * @author xiaodong.wang
 * @email wangxiaodong@beyondcent.com
 * @date 2023/12/1 16:08
 * @since x.x.x
 */
@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
@EqualsAndHashCode
@Accessors(chain = true)
@TableName("artifact_sync_slave_record")
@ApiModel("制品同步从记录")
public class ArtifactSyncSlaveRecord implements Serializable {
    private static final long serialVersionUID = 1L;



    @TableId
    @ApiModelProperty("id")
    @Column(name = "id")
    private Long id;

    
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
     * 制品同步编号
     */
    @ApiModelProperty("制品同步编号")
    @Column(name = "sync_no")
    private String syncNo;
    /**
     * 同步模式（1：推；2：拉）
     * {@linkplain  com.veadan.folib.enums.ArtifactSyncRecordSyncModelEnum }
     */
    @ApiModelProperty("同步模式（1：推；2：拉）")
    @Column(name = "sync_model")
    private Integer syncModel;
    /**
     * 同步状态（1：就绪；2：同步中；3：成功；4：失败）
     * {@linkplain com.veadan.folib.enums.ArtifactSyncRecordStatusEnum }
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

    @ApiModelProperty("临时ID，用于返回给请求端")
    @Transient
    @TableField(exist = false)
    private String tempId;

    @ApiModelProperty("文件大小")
    @Column(name = "file_size")
    private long fileSize;
}
