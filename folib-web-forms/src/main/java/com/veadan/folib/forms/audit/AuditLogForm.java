package com.veadan.folib.forms.audit;

import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

import java.util.Date;

/**
 * @author huayanjun
 * @since 2024-08-14 15:51
 */
@Data
@ApiModel("审计日志模型")
public class AuditLogForm {

    @ApiModelProperty("事件模块")
    private String moduleValue;
    @ApiModelProperty("事件名")
    private String eventValue;

    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    @ApiModelProperty("起始时间")
    private Date fromDate;

    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    @ApiModelProperty("结束时间")
    private Date toDate;
    private Integer pageNumber=1;
    private Integer pageSize=10;
}
