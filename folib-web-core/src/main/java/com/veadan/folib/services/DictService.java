package com.veadan.folib.services;

import com.veadan.folib.entity.Dict;
import com.veadan.folib.forms.dict.DictForm;
import com.veadan.folib.scanner.common.msg.TableResultResponse;

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
