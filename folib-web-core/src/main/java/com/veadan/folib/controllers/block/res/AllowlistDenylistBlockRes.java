package com.veadan.folib.controllers.block.res;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;
import org.springframework.format.annotation.DateTimeFormat;

import java.util.Date;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
@Accessors(chain = true)
@ApiModel(value = "黑白名单阻断响应",description = "")
public class AllowlistDenylistBlockRes {

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
    @ApiModelProperty(name = "创建人", notes = "")
    private String createdBy;
    /**
     * 创建时间
     */
    @ApiModelProperty(name = "创建时间", notes = "")
    private Date createdTime;
    /**
     * 更新人
     */
    @ApiModelProperty(name = "更新人", notes = "")
    private String updatedBy;
    /**
     * 更新时间
     */
    @ApiModelProperty(name = "更新时间", notes = "")
    private Date updateTime;
}
