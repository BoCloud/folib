package com.veadan.folib.entity;

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
import java.util.Date;

/**
 * 审计日志记录
 *
 * @author huayanjun
 * @since 2024-08-12 15:52
 */
@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
@Accessors(chain = true)
@Table(name = "audit_log_record")
@ApiModel("审计日志记录")
public class AuditLogRecord {

    @Id
    @GeneratedValue(generator = "JDBC", strategy = GenerationType.IDENTITY)
    @ApiModelProperty("事件id")
    @Column(name = "id")
    private Long id;


    @ApiModelProperty("事件模块")
    private String module;

    private String moduleName;

    @ApiModelProperty("事件名称")
    private String name;

    private String eventName;

    @ApiModelProperty("操作用户")
    private String username;

    @ApiModelProperty("事件对象")
    private String target;

    @ApiModelProperty("事件结果1-成功 0-失败")
    private Integer result;


    @ApiModelProperty("请求报文")
    private String request;

    @ApiModelProperty("请求结果")
    private String response;

    /**
     * 创建时间
     */
    @ApiModelProperty("创建时间")
    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    private Date createTime = new Date();
}
