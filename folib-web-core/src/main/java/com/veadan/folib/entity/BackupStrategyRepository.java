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
import javax.persistence.Id;
import javax.persistence.Table;
import java.io.Serializable;
import java.util.Date;
/**
 * @author veadan
 * @date 2024/12/17
 **/
@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
@Accessors(chain = true)
@Table(name = "backup_strategy_repository")
@ApiModel("BackupStrategyRepository")
public class BackupStrategyRepository implements Serializable {
    private static final long serialVersionUID = 1L;

    /**
     * 主键ID
     */
    @Id
    @ApiModelProperty("主键ID")
    @Column(name = "id")
    private Long id;

    /**
     * 备份策略ID
     */
    @ApiModelProperty("备份策略ID")
    @Column(name = "backup_strategy_id")
    private Long backupStrategyId;

    /**
     * 存储空间名称
     */
    @ApiModelProperty("存储空间名称")
    @Column(name = "storage_id")
    private String storageId;

    /**
     * 仓库名称
     */
    @ApiModelProperty("仓库名称")
    @Column(name = "repository_id")
    private String repositoryId;

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