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
package com.folib.enums;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;

/**
 * @author veadan
 * @since 2024-12-27 09:44
 */
@AllArgsConstructor
@NoArgsConstructor
@Getter
public enum MigrateStatusEnum {

    INITIAL(0, "初始"),
    QUEUING(1, "排队"),
    FETCHING_INDEX(2, "获取索引"),
    SYNCING_ARTIFACT(3, "同步制品"),
    PAUSED(4, "暂停"),
    COMPLETED(5, "完成"),
    INDEX_FAILED(6, "同步索引失败"),
    SYNCING_FAILED(7,"同步制品失败"),
    END(8, "结束");


    private int status;

    private String message;





}
