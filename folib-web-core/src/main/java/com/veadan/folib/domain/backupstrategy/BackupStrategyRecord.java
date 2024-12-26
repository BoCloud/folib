package com.veadan.folib.domain.backupstrategy;

import com.fasterxml.jackson.annotation.JsonFormat;
import com.veadan.folib.entity.BackupStrategyRepository;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.util.Date;
import java.util.List;

/**
 * @author leipenghui
 **/
@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
@Accessors(chain = true)
@ApiModel("BackupStrategyRecord")
public class BackupStrategyRecord implements Serializable {
    private static final long serialVersionUID = 1L;

    /**
     * id
     */
    @ApiModelProperty("id")
    private String id;

    /**
     * 是否启用 1 启用 0 不启用
     */
    @ApiModelProperty("是否启用 1 启用 0 不启用")
    private Boolean enabled;

    /**
     * 备份策略名称
     */
    @ApiModelProperty("备份策略名称")
    private String strategyName;

    /**
     * cron定时设置
     */
    @ApiModelProperty("cron定时设置")
    private String cronExpression;

    /**
     * 备份路径
     */
    @ApiModelProperty("备份路径")
    private String backupPath;

    /**
     * 增量备份 1 是 0 否
     */
    @ApiModelProperty("增量备份 1 是 0 否")
    private Boolean incremental;

    /**
     * 全量备份保留期限
     */
    @ApiModelProperty("全量备份保留期限")
    private Integer retentionPeriod;

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
     * 备份策略仓库信息
     */
    @ApiModelProperty("备份策略仓库信息")
    private List<BackupStrategyRepository> backupStrategyRepositories;
}
