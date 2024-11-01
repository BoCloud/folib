package com.veadan.folib.domain.blockstrategy;

import com.veadan.folib.entity.BlockStrategyInfo;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

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
@ApiModel("BlockStrategyRecord")
public class BlockStrategyRecord implements Serializable {
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
    private String blockStrategyName;

    /**
     * 漏洞阻断级别
     */
    @ApiModelProperty("漏洞阻断级别")
    private String vulnerabilityLevels;

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
     * 阻断license、包名
     */
    @ApiModelProperty("阻断license、包名")
    private List<BlockStrategyInfo> blockStrategyInfos;
}
