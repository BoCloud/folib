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
package com.folib.scanner.entity;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

import javax.persistence.Column;
import javax.persistence.Id;
import javax.persistence.Table;
import java.io.Serializable;


/**
 * 
 *
 * @author Veadan
 * @email xuxinping@126.com
 * @date 2022-06-03 14:51:22
 */
@Data
@Accessors(chain = true)
@Table(name = "scan_rules")
@ApiModel("scan_rules")
public class ScanRules implements Serializable {
private static final long serialVersionUID = 1L;

	/**
	 * id
	 */
	@Id
	@Column(name = "id")
	private String id;

	/**
	 * 仓库名称
	 */
	@ApiModelProperty("仓库名称")
	@Column(name = "repository")
	private String repository;

	/**
	 * 存储空间
	 */
	@ApiModelProperty("存储空间")
	@Column(name = "storage")
	private String storage;

	/**
	 * 是否扫描
	 */
	@ApiModelProperty("是否扫描")
	@Column(name = "on_scan")
	private Boolean onScan;

	/**
	 * bom扫描是否开启
	 */
	@ApiModelProperty("bom扫描是否开启")
	@Column(name = "bom_on_scan")
	private Boolean bomOnScan;

	/**
	 * 父项目id
	 */
	@ApiModelProperty("父项目id")
	@Column(name = "project_uuid")
	private String projectUuid;

	/**
	 * 扫描规则
	 */
	@ApiModelProperty("扫描规则")
	@Column(name = "scan_rule")
	private String scanRule;

	/**
	 * layout
	 */
	@ApiModelProperty("layout")
	@Column(name = "layout")
	private String layout;
	

}
