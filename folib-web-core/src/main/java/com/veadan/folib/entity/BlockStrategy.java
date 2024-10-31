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
 * @author leipenghui
 */
@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
@Accessors(chain = true)
@Table(name = "block_strategy")
@ApiModel("BlockStrategy")
public class BlockStrategy implements Serializable {
    private static final long serialVersionUID = 1L;

    /**
     * 主键ID
     */
    @Id
    @ApiModelProperty("主键ID")
    @Column(name = "id")
    private Long id;

    /**
     * 阻断策略名称
     */
    @ApiModelProperty("阻断策略名称")
    @Column(name = "block_strategy_name")
    private String blockStrategyName;

    /**
     * 漏洞阻断级别
     */
    @ApiModelProperty("漏洞阻断级别")
    @Column(name = "vulnerability_levels")
    private String vulnerabilityLevels;

    /**
     * 过滤漏洞白名单
     */
    @ApiModelProperty("过滤漏洞白名单")
    @Column(name = "filter_vulnerability_whites")
    private Boolean filterVulnerabilityWhites;

    /**
     * 过滤漏洞黑名单
     */
    @ApiModelProperty("过滤漏洞黑名单")
    @Column(name = "filter_vulnerability_blacks")
    private Boolean filterVulnerabilityBlacks;

    /**
     * 过滤license白名单
     */
    @ApiModelProperty("过滤license白名单")
    @Column(name = "filter_license_whites")
    private Boolean filterLicenseWhites;

    /**
     * 过滤license黑名单
     */
    @ApiModelProperty("过滤license黑名单")
    @Column(name = "filter_license_blacks")
    private Boolean filterLicenseBlacks;

    /**
     * 全量包名
     */
    @ApiModelProperty("全量包名")
    @Column(name = "filter_all_package_name")
    private Boolean filterAllPackageName;

    /**
     * 全量license
     */
    @ApiModelProperty("全量license")
    @Column(name = "filter_all_license")
    private Boolean filterAllLicense;

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
