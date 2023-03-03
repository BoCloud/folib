package com.veadan.folib.services;

import com.veadan.folib.entity.Dict;

import java.util.List;

/**
 * @author leipenghui
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
    void updateDict(Dict dict);

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
}
