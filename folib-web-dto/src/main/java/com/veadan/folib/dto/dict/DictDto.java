package com.veadan.folib.dto.dict;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;
import java.util.Date;


/**
 * @author leipenghui
 */
@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
@ApiModel("dict")
public class DictDto implements Serializable {
    private static final long serialVersionUID = 1L;

    /**
     * id
     */
    @ApiModelProperty("id")
    private Long id;

    /**
     * 创建时间
     */
    @ApiModelProperty("创建时间")
    private Date createTime;

    /**
     * 字典类型
     */
    @ApiModelProperty("字典类型")
    private String dictType;

    /**
     * 字典key
     */
    @ApiModelProperty("字典key")
    private String dictKey;

    /**
     * 字典值
     */
    @ApiModelProperty("字典值")
    private String dictValue;

    /**
     * 别名
     */
    @ApiModelProperty("别名")
    private String alias;

    /**
     * 备注
     */
    @ApiModelProperty("备注")
    private String comment;

    /**
     * 是否覆盖系统属性
     */
    @ApiModelProperty("是否覆盖系统属性 true 覆盖")
    private Boolean overrideSystemProperty;
}
