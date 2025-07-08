package com.folib.model.request;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

@Data
@Accessors(chain = true)
@ApiModel(description = "ohpm登录请求")
public class OhpmLoginReq {

    @ApiModelProperty("发布ID")
    private String publishId;

    @ApiModelProperty("时间戳")
    private Long timestamp;

    @ApiModelProperty("随机字符串")
    private String nonce;

    @ApiModelProperty("签名")
    private String signature;

    @ApiModelProperty("版本号")
    private String version;


}
