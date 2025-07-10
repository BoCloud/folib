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
package com.folib.domain.migrate;

import com.folib.constant.GlobalConstants;
import lombok.Data;
import org.apache.commons.lang3.StringUtils;

/**
 * @author veadan
 * @since 2024-12-24 10:56
 */
@Data
public class ArtifactMigrateInfo {
    // 迁移id
    private String migrateId;
    // 迁移仓库数
    private int total;
    // 浏览器前缀
    private String browsePrefix;
    // 单实例并发仓库数
    private int batchSize ;
    // 单仓库迁移线程数 （默认cpu核心数*2）
    private int threadNumber;
    // 迁移状态 0-初始
    private int status;

    private String username;

    private String password;

    private String remotePreUrl;

    private Integer syncMeta;

    private Integer webhookSetting;

    public void setBrowsePrefix(String browsePrefix){
        this.browsePrefix= StringUtils.removeEnd(browsePrefix, GlobalConstants.SEPARATOR);
    }

    public void setRemotePreUrl(String remotePreUrl){
        this.remotePreUrl= StringUtils.removeEnd(remotePreUrl, GlobalConstants.SEPARATOR);
    }


}
