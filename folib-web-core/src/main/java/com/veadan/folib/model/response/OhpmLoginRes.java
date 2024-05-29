package com.veadan.folib.model.response;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Builder;
import lombok.Data;
import lombok.experimental.Accessors;

@Data
@Accessors(chain = true)
@Builder
@ApiModel(description = "ohpm登录响应")
public class OhpmLoginRes {

    @ApiModelProperty("是否成功")
    private boolean success;

    @ApiModelProperty("token")
    private String token;

    @ApiModelProperty("消息")
    private String message;

}
