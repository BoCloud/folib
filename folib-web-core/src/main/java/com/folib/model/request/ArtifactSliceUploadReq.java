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
package com.folib.model.request;

import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import org.springframework.web.multipart.MultipartFile;

import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.NotNull;
import java.util.Map;

/**
 * @author veadan
 * @date 2023/11/22 14:39
 */
@Data
public class ArtifactSliceUploadReq {
    @ApiModelProperty("存储ID")
    @NotEmpty(message = "存储ID不能为空")
    private String storageId;
    @ApiModelProperty("仓库ID")
    @NotEmpty(message = "仓库ID不能为空")
    private String repositoryId;
    @ApiModelProperty("Folib存储路径（不能以`/`开头）")
    @NotEmpty(message = "Folib存储路径不能为空")
    private String path;
    @ApiModelProperty("切片文件")
    @NotNull(message = "切片文件不能为空")
    private MultipartFile file;
    @ApiModelProperty("切片文件合并ID")
    @NotEmpty(message = "切片文件合并ID不能为空")
    private String mergeId;
    @ApiModelProperty("切片当前块数")
    @NotNull(message = "切片当前块数不能为空")
    private Integer chunkIndex;
    @ApiModelProperty("切片最大块数")
    @NotNull(message = "切片最大块数不能为空")
    private Integer chunkIndexMax;
    @ApiModelProperty("切片文件原始MD5（非切片文件的MD5）")
    @NotEmpty(message = "切片文件原始MD5不能为空")
    private String originFileMd5;
    @ApiModelProperty("切片文件MD5")
    private String sliceMd5;
    @ApiModelProperty("制品元数据")
    private Map<String, Object> metaData;
}
