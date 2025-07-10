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
package com.folib.controllers.adapter.jfrog.dto;

import lombok.Data;

/**
 * docker 制品晋级的参数接收实体
 */
@Data
public class DockerCopyDto {
    private String targetStorageId;
    // The target repository for the move or copy
    private String  targetRepo;
    // The image name to promote
    private String dockerRepository ;
    // An optional docker repository name, if null, will use the same name as 'dockerRepository'
    private String  targetDockerRepository;
    // An optional tag name to promote, if null - the entire docker repository will be promoted. Available from v4.10.
    private String  tag ;
    // An optional target tag to assign the image after promotion, if null - will use the same tag
    private String  targetTag ;
    //  是否拷贝为false
    private Boolean  copy= false;
}
