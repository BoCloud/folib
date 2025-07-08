package com.folib.entity;

import com.baomidou.mybatisplus.annotation.TableField;
import com.baomidou.mybatisplus.annotation.TableName;
import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import javax.persistence.Column;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.Table;
import java.io.Serializable;
import java.util.Date;

/**
 * @author veadan
 * @since 2024-08-20 13:41
 */
@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
@Accessors(chain = true)
@TableName("access_token")
@ApiModel("访问令牌")
public class AccessToken implements Serializable,Cloneable {


    @TableField("id")
    private long id;


    @TableField( "token_id")
    @ApiModelProperty("令牌标识")
    private String tokenId;

    @ApiModelProperty("描述")
    private String description;


    @ApiModelProperty("操作人")
    private String operator;


    @ApiModelProperty("过期时间")
    @TableField( "expire_time")
    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    private Date expireTime;


    @ApiModelProperty("用户")
    private String username;

    @ApiModelProperty("创建时间")
    @TableField( "create_time")
    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    private Date createTime;
}
