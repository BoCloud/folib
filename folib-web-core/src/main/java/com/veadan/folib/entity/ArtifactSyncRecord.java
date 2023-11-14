package com.veadan.folib.entity;

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
import javax.persistence.Id;
import javax.persistence.Table;
import java.io.Serializable;
import java.util.Date;


/**
 *
 * @author xiaodong.wang
 * @email wangxiaodong@beyondcent.com
 * @date 2023/10/5 17:04
 * @since x.x.x
 */
@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
@Accessors(chain = true)
@Table(name = "artifact_sync_record")
@ApiModel("ArtifactSyncRecord")
public class ArtifactSyncRecord implements Serializable {
    private static final long serialVersionUID = 1L;


    @Id
    @GeneratedValue(generator = "JDBC",strategy = GenerationType.IDENTITY)
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
     * 制品操作（1：制品晋级；2：制品分发）
     * {@linkplain com.veadan.folib.enums.ArtifactSyncRecordOpsTypeEnum }
     */
    @ApiModelProperty("制品操作（1：制品晋级；2：制品分发）")
    @Column(name = "ops_type")
    private Integer opsType;
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
    @Column(name = "created_by")
    private String createdBy;
    /**
     * 创建时间
     */
    @ApiModelProperty("创建时间")
    @Column(name = "created_time")
    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    private Date createdTime;
    /**
     * 更新人
     */
    @ApiModelProperty("更新人")
    @Column(name = "updated_by")
    private String updatedBy;
    /**
     * 更新时间
     */
    @ApiModelProperty("更新时间")
    @Column(name = "updated_time")
    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    private Date updatedTime;
}
