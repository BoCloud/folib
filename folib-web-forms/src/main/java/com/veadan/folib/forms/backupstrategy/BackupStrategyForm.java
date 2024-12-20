package com.veadan.folib.forms.backupstrategy;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import javax.validation.Valid;
import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotEmpty;
import java.io.Serializable;
import java.util.List;

/**
 * @author leipenghui
 **/
@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
@Accessors(chain = true)
@ApiModel("BlockStrategyForm")
public class BackupStrategyForm implements Serializable {
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
    @NotBlank(message = "请填写备份策略名称", groups = {SaveGroup.class, UpdateGroup.class, DeleteGroup.class, ExecuteGroup.class})
    private String strategyName;

    /**
     * 模糊备份策略名称
     */
    private String matchStrategyName;

    /**
     * cron定时设置
     */
    @ApiModelProperty("cron定时设置")
    @NotBlank(message = "请填写cron定时设置", groups = {SaveGroup.class, UpdateGroup.class})
    private String cronExpression;

    /**
     * 备份路径
     */
    @ApiModelProperty("备份路径")
    @NotBlank(message = "请填写备份路径", groups = {SaveGroup.class, UpdateGroup.class})
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
     * 储存空间名称
     */
    private String storageId;

    /**
     * 仓库名称
     */
    private String repositoryId;

    /**
     * 模糊储存空间名称
     */
    private String matchStorageId;

    /**
     * 模糊仓库名称
     */
    private String matchRepositoryId;

    /**
     * 仓库列表
     */
    @ApiModelProperty("仓库列表")
    @Valid
    @NotEmpty(message = "请传入仓库列表", groups = {SaveGroup.class, UpdateGroup.class})
    private List<String> repositories;

    public interface SaveGroup
            extends Serializable {
        // 新增组
    }

    public interface UpdateGroup
            extends Serializable {
        // 更新组
    }

    public interface DeleteGroup
            extends Serializable {
        // 删除组
    }

    public interface ExecuteGroup
            extends Serializable {
        // 执行组
    }
}
