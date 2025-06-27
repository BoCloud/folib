package com.veadan.folib.mapper;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.veadan.folib.entity.Dict;
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
