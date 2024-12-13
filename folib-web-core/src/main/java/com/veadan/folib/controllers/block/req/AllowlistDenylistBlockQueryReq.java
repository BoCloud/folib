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
@ApiModel(value = "黑白名单阻断查询请求",description = "")
public class AllowlistDenylistBlockQueryReq {

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

    private String category;

    private String tag;

    private  int page;

    private  int size;
}
