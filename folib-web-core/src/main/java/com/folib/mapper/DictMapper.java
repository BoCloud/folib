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
package com.folib.mapper;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.folib.entity.Dict;
import org.apache.ibatis.annotations.Param;
import org.springframework.stereotype.Component;

import java.util.List;

/**
 * @author veadan
 */
@Component
public interface DictMapper extends BaseMapper<Dict> {

    /**
     * 根据条件查询字典
     *
     * @param dict dict
     * @return 字典
     */
    Dict selectOneDict(Dict dict);

    /**
     * 删除上传进度的历史数据
     *
     * @param seconds 秒数
     * @return 受影响条数
     */
    int deleteHistoryDataForUploadProcessBySeconds(int seconds);

    /**
     * 批量保存字典数据
     * @param dictList 字典列表
     */
    void batchInsertDict(@Param("dictList") List<Dict> dictList);

}
