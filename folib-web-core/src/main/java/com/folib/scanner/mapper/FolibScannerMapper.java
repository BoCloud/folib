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
package com.folib.scanner.mapper;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.folib.scanner.common.base.BaseQuery;
import com.folib.scanner.entity.*;
import org.apache.ibatis.annotations.Param;

import java.util.List;

/**
 * @author Veadan
 * @version 2022-05-31 23:12:54
 * @email xuxinping@126.com
 */
public interface FolibScannerMapper extends BaseMapper<FolibScanner> {
    //=#{enableScan} where repository=#{repository} and storage=#{storage}
    public void updateByStorage(@Param("enableScan") boolean enableScan, @Param("repository") String repository, @Param("storage") String storage);

    List<FolibScanner> selectEnableScan();

    void updateScaning();

    ScanSumVo getScanSum(@Param("baseQuery") BaseQuery baseQuery);

    List<ScannerSumDifVo> getScannerSumDifVoList(@Param("baseQuery") BaseQuery baseQuery);

    List<ScanSumByDate> mounthDayCount(@Param("baseQuery") BaseQuery baseQuery);

    List<ScanSumByDate> weekDayCount(@Param("baseQuery") BaseQuery baseQuery);

    ScanSumByDate getCountByDayOne(@Param("dateNum") int dateNum, @Param("baseQuery") BaseQuery baseQuery);

    /**
     * 根据制品目录进行批量删除
     *
     * @param storage    存储空间
     * @param repository 仓库名称
     * @param path       目录
     */
    void deleteByPathLike(@Param("storage") String storage, @Param("repository") String repository, @Param("path") String path);

    /**
     * 统计数量
     *
     * @param folibScanner 扫描对象
     * @param baseQuery    基础查询对象
     * @return 统计数量
     */
    int selectFolibScannerCount(@Param("folibScanner") FolibScanner folibScanner, @Param("baseQuery") BaseQuery baseQuery);

    /**
     * 查询docker布局扫描报告
     *
     * @param storage      存储空间
     * @param repository   仓库名称
     * @param artifactName 制品名称
     * @return docker布局扫描报告
     */
    List<FolibScannerDockerTableVO> selectDockerList(@Param("storage") String storage, @Param("repository") String repository, @Param("artifactName") String artifactName);

    /**
     * 查询docker布局子表信息
     *
     * @param storage    存储空间
     * @param repository 仓库名称
     * @param version    docker版本路径
     * @return docker布局子表信息
     */
    List<FolibScanner> selectDockerChildList(@Param("storage") String storage, @Param("repository") String repository, @Param("version") String version);

}
