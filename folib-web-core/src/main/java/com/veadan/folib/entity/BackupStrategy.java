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
@Table(name = "backup_strategy")
@ApiModel("BackupStrategy")
public class BackupStrategy implements Serializable {
    private static final long serialVersionUID = 1L;

    /**
     * 主键ID
     */
    @Id
    @ApiModelProperty("主键ID")
    @Column(name = "id")
    private Long id;

    /**
     * 是否启用 1 启用 0 不启用
     */
    @ApiModelProperty("是否启用 1 启用 0 不启用")
    @Column(name = "enabled")
    private Boolean enabled;

    /**
     * 备份策略名称
     */
    @ApiModelProperty("备份策略名称")
    @Column(name = "strategy_name")
    private String strategyName;

    /**
     * cron定时设置
     */
    @ApiModelProperty("cron定时设置")
    @Column(name = "cron_expression")
    private String cronExpression;

    /**
     * 备份路径
     */
    @ApiModelProperty("备份路径")
    @Column(name = "backup_path")
    private String backupPath;

    /**
     * 增量备份 1 是 0 否
     */
    @ApiModelProperty("增量备份 1 是 0 否")
    @Column(name = "incremental")
    private Boolean incremental;

    /**
     * 全量备份保留期限
     */
    @ApiModelProperty("全量备份保留期限")
    @Column(name = "retention_period")
    private Integer retentionPeriod;

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