/*
 * Folib - [新一代AI制品仓库]
 * Copyright (C) 2025 bocloud.com.cn <folib@beyondcent.com>
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * 本程序是自由软件：您可依据GNU通用公共许可证（GPL-3.0+）条款重新发布和修改，
 * 但禁止任何形式的商业售卖行为（包括但不限于：直接销售、捆绑销售、云服务商用）。
 *
 * This program is distributed WITHOUT ANY WARRANTY.
 * Commercial sale of this software is expressly prohibited.
 *
 * For license details, see: https://www.gnu.org/licenses/gpl-3.0.html
 * 商业授权咨询请联系：folib@beyondcent.com
 */
package com.folib.entity;

import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import javax.persistence.Column;
import javax.persistence.Id;
import javax.persistence.Table;
import java.io.Serializable;
import java.util.Date;


/**
 * @author leipenghui
 */
@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
@Accessors(chain = true)
@Table(name = "webhook_log")
@ApiModel("WebhookLog")
public class WebhookLog implements Serializable {
    private static final long serialVersionUID = 1L;

    /**
     * id
     */
    @Id
    @ApiModelProperty("id")
    @Column(name = "id")
    private Long id;

    /**
     * 创建时间
     */
    @ApiModelProperty("创建时间")
    @Column(name = "create_time")
    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    private Date createTime;

    /**
     * 事件类型
     */
    @ApiModelProperty("事件类型")
    @Column(name = "event_type")
    private String eventType;

    /**
     * 存储空间名称
     */
    @ApiModelProperty("存储空间名称")
    @Column(name = "storage_id")
    private String storageId;

    /**
     * 仓库名称
     */
    @ApiModelProperty("仓库名称")
    @Column(name = "repository_id")
    private String repositoryId;

    /**
     * 制品路径
     */
    @ApiModelProperty("制品路径")
    @Column(name = "artifact_path")
    private String artifactPath;

    /**
     * 请求url
     */
    @ApiModelProperty("请求url")
    @Column(name = "url")
    private String url;

    /**
     * 访问令牌
     */
    @ApiModelProperty("访问令牌")
    @Column(name = "access_token")
    private String accessToken;

    /**
     * 请求方式
     */
    @ApiModelProperty("请求方式")
    @Column(name = "method")
    private String method;

    /**
     * 完成时间（秒）
     */
    @ApiModelProperty("完成时间（秒）")
    @Column(name = "completion_time")
    private String completionTime;

    /**
     * 请求头
     */
    @ApiModelProperty("请求头")
    @Column(name = "request_headers")
    private String requestHeaders;

    /**
     * 请求报文
     */
    @ApiModelProperty("请求报文")
    @Column(name = "request")
    private String request;

    /**
     * 响应状态码
     */
    @ApiModelProperty("响应状态码")
    @Column(name = "response_status")
    private String responseStatus;

    /**
     * 响应头
     */
    @ApiModelProperty("响应头")
    @Column(name = "response_headers")
    private String responseHeaders;

    /**
     * 响应报文
     */
    @ApiModelProperty("响应报文")
    @Column(name = "response")
    private String response;

    /**
     * 备注
     */
    @ApiModelProperty("备注")
    @Column(name = "remark")
    private String remark;
}
