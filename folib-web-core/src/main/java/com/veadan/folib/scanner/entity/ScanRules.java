package com.veadan.folib.scanner.entity;

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

		//
	@Id
	@Column(name = "id")
	private String id;
	
		//仓库名称
	@ApiModelProperty("仓库名称")
	@Column(name = "repository")
	private String repository;
	
		//存储空间
	@ApiModelProperty("存储空间")
	@Column(name = "storage")
	private String storage;
	
		//是否扫描
	@ApiModelProperty("是否扫描")
	@Column(name = "on_scan")
	private boolean onScan;
	
		//扫描规则
	@ApiModelProperty("扫描规则")
	@Column(name = "scan_rule")
	private String scanRule;

	@ApiModelProperty("layout")
	@Column(name = "layout")
	private String layout;
	

}
