package com.veadan.folib.mapper;

import com.veadan.folib.entity.Dict;
import com.veadan.folib.scanner.common.base.CommonMapper;
import org.springframework.stereotype.Component;

/**
 * @author leipenghui
 */
@Component
public interface DictMapper extends CommonMapper<Dict> {

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

}
