package com.folib.model.request;

import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModelProperty;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.math.BigDecimal;
import java.util.Date;
import java.util.List;

/**
 * @author veadan
 * @date 2024/7/18
 **/
@Data
@AllArgsConstructor
@NoArgsConstructor
@Accessors(chain = true)
public class PageReq implements Serializable {
    private static final long serialVersionUID = 1L;

    /**
     * 页码
     */
    @ApiModelProperty("页码")
    private Integer page;

    /**
     * 每页数量
     */
    @ApiModelProperty("每页数量")
    private Integer limit;

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

