package com.veadan.folib.controllers.block.req;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.util.Date;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
@Accessors(chain = true)
@ApiModel(value = "黑白名单阻断请求",description = "")
public class AllowlistDenylistBlockReq {

    private Long id;
    @ApiModelProperty(name = "标识具体规则如漏洞ID", notes = "")
    private String identifier;
    /**
     * 类型[whites:白名单,blacklist：黑名单]
     */
    @ApiModelProperty(name = "类型[whites:白名单,blacklist：黑名单]", notes = "")
    private String type;
    /**
     * 有效期
     */
    @ApiModelProperty(name = "有效期", notes = "")
    private Date validFrom;


    @ApiModelProperty(name = "类别", notes = "")
    private String category;


    @ApiModelProperty(name = "标识", notes = "")
    private String tag;
    /**
     * 业务域[SYSTEM:系统，REPOSITORY:仓库]
     */
    @ApiModelProperty(name = "业务域", notes = "[PLATFORM:平台，REPOSITORY:仓库]")
    private String domain;

    /**
     * 关联ID[仓库ID]
     */
    @ApiModelProperty(name = "关联ID", notes = "[仓库ID]")
    private String correlationId;

    /**
     * 创建人
     */
    private String createdBy;


    /**
     * 更新人
     */
    private String updatedBy;


    /**
     * 创建时间
     */
    private Date createdTime;

    /**
     * 更新时间
     */
    private Date updateTime;


}
