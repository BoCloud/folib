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
     *
     * @param idList id列表
     */
    void batchDelete(@Param("idList") @NotNull List<Long> idList);

    /**
     * 游标列表
     *
     * @param nodeId    nodeId
     * @param lastId    lastId
     * @param batchSize batchSize
     */
    List<ArtifactCacheRecord> selectArtifactCacheRecordByCursor(@Param("nodeId") String nodeId, @Param("lastId") Long lastId, @Param("batchSize") int batchSize);
}
