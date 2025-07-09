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
package com.folib.services;

import com.folib.forms.component.ArtifactGraphForm;
import com.folib.forms.component.ArtifactStatisticsForm;
import com.folib.forms.component.ComponentTableForm;
import com.folib.forms.vulnerability.AffectedArtifactsForm;
import com.folib.scanner.common.msg.TableResultResponse;

/**
 * @author veadan
 * @date 2023/5/24
 **/
public interface ComponentService {

    /**
     * 分页查询组件列表
     *
     * @param page          页码
     * @param limit         每页数量
     * @param name          组件名称
     * @param groupId       组名称
     * @param version       版本号
     * @param searchKeyword 搜索关键词
     * @return 组件列表
     */
    TableResultResponse<ComponentTableForm> queryComponentPage(Integer page, Integer limit, String name, String groupId, String version, String searchKeyword);

    /**
     * 根据制品分页查询组件列表
     *
     * @param page          页码
     * @param limit         每页数量
     * @param artifactPath  制品uuid
     * @param searchKeyword 搜索关键词
     * @return 组件列表
     */
    TableResultResponse<ComponentTableForm> queryComponentPageByArtifact(Integer page, Integer limit, String artifactPath, String searchKeyword);

    /**
     * 查询组件信息
     *
     * @param uuid uuid
     * @return 组件信息
     */
    ComponentTableForm queryComponentOne(String uuid);

    /**
     * 根据组件id分页查询关联制品
     *
     * @param page          页码
     * @param limit         每页数量
     * @param componentUuid 组件id
     * @param searchKeyword 搜索关键词
     * @return 制品列表
     */
    TableResultResponse<AffectedArtifactsForm> queryArtifactByComponentUuid(Integer page, Integer limit, String componentUuid, String searchKeyword);

    /**
     * 组件关联的制品图谱展示
     *
     * @param componentUuid 组件id
     * @return 数据
     */
    ArtifactGraphForm artifactGraph(String componentUuid);

    /**
     * 组件关联的制品统计数据
     *
     * @param componentUuid 组件id
     * @return 数据
     */
    ArtifactStatisticsForm artifactStatistics(String componentUuid);
}
