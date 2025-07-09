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
package com.folib.forms.externalnode;

import com.fasterxml.jackson.annotation.JsonFormat;
import com.folib.enums.ArtifactoryRepositoryTypeEnum;
import com.folib.forms.validate.DeleteGroup;
import com.folib.forms.validate.SaveGroup;
import com.folib.forms.validate.UpdateGroup;
import com.folib.validation.externalnode.ArtifactoryRepositoryEnumValue;
import io.swagger.annotations.ApiModelProperty;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;
import org.hibernate.validator.constraints.Length;

import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotNull;
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
public class ExternalNodeForm implements Serializable {
    private static final long serialVersionUID = 1L;

    /**
     * 主键id
     */
    @ApiModelProperty("id")
    @NotNull(message = "请填写id", groups = {UpdateGroup.class, DeleteGroup.class})
    private Long id;

    /**
     * 创建时间
     */
    @ApiModelProperty("创建时间")
    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    private Date createTime;

    /**
     * 节点名称
     */
    @ApiModelProperty("节点名称")
    @NotBlank(message = "请填写节点名称", groups = {SaveGroup.class})
    @Length(max = 50, message = "节点名称不能超过50个字符")
    private String nodeName;

    /**
     * 制品库类型
     */
    @ApiModelProperty("制品库类型")
    @ArtifactoryRepositoryEnumValue(message = "制品库类型校验失败，请检查制品库类型.", type = ArtifactoryRepositoryTypeEnum.class)
    private String type;

    /**
     * 节点地址
     */
    @ApiModelProperty("节点地址")
    @NotBlank(message = "请填写节点地址", groups = {SaveGroup.class})
    @Length(max = 255, message = "节点地址不能超过255个字符")
    private String address;

    /**
     * 用户名
     */
    @ApiModelProperty("用户名")
    @NotBlank(message = "请填写用户名", groups = {SaveGroup.class})
    @Length(max = 50, message = "用户名不能超过50个字符")
    private String username;

    /**
     * 密码
     */
    @ApiModelProperty("密码")
    @NotBlank(message = "请填写密码", groups = {SaveGroup.class})
    private String password;

    /**
     * 备注
     */
    @ApiModelProperty("备注")
    private String comment;
}
