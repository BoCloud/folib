package com.veadan.folib.mapper;

import com.veadan.folib.entity.ArtifactCacheRecord;
import com.veadan.folib.scanner.common.base.CommonMapper;
import org.apache.ibatis.annotations.Param;
import org.springframework.stereotype.Component;

import javax.validation.constraints.NotNull;
import java.util.List;

/**
 * @author leipenghui
 */
@Component
public interface ArtifactCacheRecordMapper extends CommonMapper<ArtifactCacheRecord> {

    /**
     * 批量删除
     * @param idList id列表
     */
    void batchDelete(@Param("idList") @NotNull List<Long> idList);

}
