package com.veadan.folib.model.request;

import io.swagger.annotations.ApiModelProperty;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;

/**
 * @author leipenghui
 * @date 2024/7/18
 **/
@Data
@AllArgsConstructor
@NoArgsConstructor
@Accessors(chain = true)
public class OrderByReq implements Serializable {
    private static final long serialVersionUID = 1L;

    /**
     * 排序字段
     */
    @ApiModelProperty("排序字段")
    private String sortField;

    /**
     * 排序顺序
     */
    @ApiModelProperty("排序顺序")
    private String sortOrder;
}

