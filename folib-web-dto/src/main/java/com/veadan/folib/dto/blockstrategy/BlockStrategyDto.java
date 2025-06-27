package com.veadan.folib.dto.blockstrategy;

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
 * @author veadan
 **/
@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
@Accessors(chain = true)
@ApiModel("BlockStrategyForm")
public class BlockStrategyDto implements Serializable {
    private static final long serialVersionUID = 1L;

    /**
     * id
     */
    @ApiModelProperty("id")
    private String id;

    /**
     * 阻断策略名称
     */
    @ApiModelProperty("阻断策略名称")
    @NotBlank(message = "请填写阻断策略名称", groups = {SaveGroup.class, UpdateGroup.class, DeleteGroup.class})
    private String blockStrategyName;

    /**
     * 模糊阻断策略名称
     */
    private String matchBlockStrategyName;

    /**
     * 漏洞阻断级别
     */
    @ApiModelProperty("漏洞阻断级别")
    private List<String> vulnerabilityLevels;

    /**
     * 过滤漏洞白名单
     */
    @ApiModelProperty("过滤漏洞白名单")
    private Boolean filterVulnerabilityWhites;

    /**
     * 过滤漏洞黑名单
     */
    @ApiModelProperty("过滤漏洞黑名单")
    private Boolean filterVulnerabilityBlacks;

    /**
     * 全量包名
     */
    @ApiModelProperty("全量包名")
    private Boolean filterAllPackageName;

    /**
     * 全量license
     */
    @ApiModelProperty("全量license")
    private Boolean filterAllLicense;

    /**
     * 过滤license白名单
     */
    @ApiModelProperty("过滤license白名单")
    private Boolean filterLicenseWhites;

    /**
     * 过滤license黑名单
     */
    @ApiModelProperty("过滤license黑名单")
    private Boolean filterLicenseBlacks;

    /**
     * 储存空间名称
     */
    private String storageId;

    /**
     * 仓库名称
     */
    private String repositoryId;

    /**
     * 包名称
     */
    private String packageName;

    /**
     * license id
     */
    private String licenseId;

    /**
     * 模糊储存空间名称
     */
    private String matchStorageId;

    /**
     * 模糊仓库名称
     */
    private String matchRepositoryId;

    /**
     * 模糊包名称
     */
    private String matchPackageName;

    /**
     * 模糊license id
     */
    private String matchLicenseId;

    /**
     * 仓库列表
     */
    @ApiModelProperty("仓库列表")
    @Valid
    @NotEmpty(message = "请传入仓库列表", groups = {SaveGroup.class, UpdateGroup.class})
    private List<String> repositories;

    /**
     * 包名列表
     */
    @ApiModelProperty("包名列表")
    private List<String> packageNames;

    /**
     * license列表
     */
    @ApiModelProperty("license列表")
    private List<String> licenses;

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
}
