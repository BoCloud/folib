package com.veadan.folib.entity;

import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import javax.persistence.*;
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
@Table(name = "license")
@ApiModel("license")
public class License implements Serializable {
    private static final long serialVersionUID = 1L;


    @Id
    @GeneratedValue(generator = "JDBC", strategy = GenerationType.IDENTITY)
    @ApiModelProperty("id")
    @Column(name = "id")
    private Long id;

    /**
     * 创建时间
     */
    @ApiModelProperty("创建时间")
    @Column(name = "create_time")
    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    private Date createTime;

    /**
     * 许可证id
     */
    @ApiModelProperty("许可证id")
    @Column(name = "license_id")
    private String licenseId;

    /**
     * 许可证名称
     */
    @ApiModelProperty("许可证名称")
    @Column(name = "license_name")
    private String licenseName;

    /**
     * 许可证地址
     */
    @ApiModelProperty("许可证地址")
    @Column(name = "license_url")
    private String licenseUrl;

    /**
     * 是否是自定义许可证 1是 0否
     */
    @ApiModelProperty("是否是自定义许可证 1是 0否")
    @Column(name = "is_custom_license")
    private Integer isCustomLicense;

    /**
     * 是否已弃用 1是 0否
     */
    @ApiModelProperty("是否已弃用 1是 0否")
    @Column(name = "is_deprecated")
    private Integer isDeprecated;

    /**
     * 是否属于OSI-Approved授权协议 1是 0否
     */
    @ApiModelProperty("是否属于OSI-Approved授权协议 1是 0否")
    @Column(name = "is_osi_approved")
    private Integer isOsiApproved;

    /**
     * 是否为自由软件基金会 1是 0否
     */
    @ApiModelProperty("是否为自由软件基金会 1是 0否")
    @Column(name = "is_fsf_libre")
    private Integer isFsfLibre;

    /**
     * 许可证头信息
     */
    @ApiModelProperty("许可证头信息")
    @Column(name = "header")
    private String header;

    /**
     * 许可证模板
     */
    @ApiModelProperty("许可证模板")
    @Column(name = "template")
    private String template;

    /**
     * 许可证原文内容
     */
    @ApiModelProperty("许可证原文内容")
    @Column(name = "content")
    private String content;

    /**
     * 许可证中文内容
     */
    @ApiModelProperty("许可证中文内容")
    @Column(name = "content_cn")
    private String contentCn;
    /**
     * 备注
     */
    @ApiModelProperty("备注")
    @Column(name = "comment")
    private String comment;
}
