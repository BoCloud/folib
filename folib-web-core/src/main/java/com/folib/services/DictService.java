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

import com.folib.entity.Dict;
import com.folib.forms.dict.DictForm;
import com.folib.scanner.common.msg.TableResultResponse;

import java.util.List;

/**
 * @author veadan
 * @date 2023/2/28
 **/
public interface DictService {

    /**
     * 新增字典
     *
     * @param dict 字典
     */
    void saveDict(Dict dict);

    /**
     * 修改字典
     *
     * @param dict 字典
     */
    void updateDict(DictForm dict);

    /**
     * 新增或更新字典
     *
     * @param dict       字典
     * @param isOverride dictValue相同值是否覆盖，默认覆盖
     */
    void saveOrUpdateDict(Dict dict, Boolean isOverride);

    /**
     * 删除字典
     *
     * @param dict 字典
     */
    void deleteDict(Dict dict);

    /**
     * 删除字典
     *
     * @param id 字典id
     */
    void deleteDictById(Long id);

    /**
     * 查询字典
     *
     * @param dict 字典
     * @return 字典列表
     */
    List<Dict> selectDict(Dict dict);

    /**
     * 查询字典
     *
     * @param dict 字典
     * @return 字典
     */
    Dict selectOneDict(Dict dict);

    /**
     * 查询字典
     *
     * @param dict 字典
     * @return 字典
     */
    Dict selectLatestOneDict(Dict dict);

    /**
     * 查询字典
     *
     * @param dict 字典
     * @return 字典
     */
    List<Dict> selectLatestListDict(Dict dict);

    /**
     * 查询未执行的升级任务
     *
     * @return 未执行的升级任务
     */
    List<Dict> selectUnExecutedTask();

    /**
     * 更新未执行的升级任务的状态
     *
     * @param dict 参数
     */
    void updateUnExecutedTask(Dict dict);

    /**
     * 批量保存字典数据
     *
     * @param dictList 字典数据
     */
    void batchInsertDict(List<Dict> dictList);


    void updateById(Dict dict);

    Dict getById(Long id);

    void saveOrUpdateByTypeAndKey(Dict dict);

    List<Dict> selectByTypeAndKey(String type, String key);

    TableResultResponse<Dict> getSystemDict(Integer page, Integer limit, String dictKey);
}
