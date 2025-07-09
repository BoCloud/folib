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

import com.folib.domain.license.LicenseBlackWhite;
import com.folib.entity.License;
import com.folib.forms.license.LicenseTableForm;
import com.folib.scanner.common.msg.TableResultResponse;

import java.util.List;

/**
 * @author veadan
 **/
public interface LicenseService {

    /**
     * 新增许可证
     *
     * @param license 许可证
     */
    void saveLicense(License license);

    /**
     * 修改许可证
     *
     * @param license 许可证
     */
    void updateLicense(License license);

    /**
     * 查询许可证
     *
     * @param license 许可证
     * @return 许可证列表
     */
    List<License> selectLicense(License license);

    /**
     * 查询许可证
     *
     * @param license 许可证
     * @return 许可证
     */
    License selectOneLicense(License license);

    /**
     * 更新许可证地址
     */
    void updateLicenseUrl();

    /**
     * 更新许可证中文内容
     */
    void updateContentCn();

    /**
     * 分页查询license列表
     *
     * @param page           页码
     * @param limit          每页数量
     * @param searchKeyword  搜索关键词
     * @param licenseId      license名称
     * @param blackWhiteType 黑白名单类型
     * @return license列表
     */
    TableResultResponse<LicenseTableForm> queryLicensePage(Integer page, Integer limit, String searchKeyword, String licenseId, Integer blackWhiteType);

    /**
     * 查询license列表
     *
     * @param searchKeyword         搜索关键词
     * @param licenseId             license名称
     * @param blackWhiteType        黑白名单类型
     * @param excludeBlackWhiteType 排除黑白名单类型
     * @return license列表
     */
    List<LicenseTableForm> queryLicense(String searchKeyword, String licenseId, Integer blackWhiteType, Integer excludeBlackWhiteType);

    /**
     * 设置黑白名单
     *
     * @param licenseBlackWhite 参数
     */
    void blackWhite(LicenseBlackWhite licenseBlackWhite);

    /**
     * 获取license缓存
     *
     * @return license缓存
     */
    List<License> getLicenseCache();
}
