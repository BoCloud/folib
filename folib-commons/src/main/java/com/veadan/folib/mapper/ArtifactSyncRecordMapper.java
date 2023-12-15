package com.veadan.folib.mapper;

import com.veadan.folib.entity.ArtifactSyncRecord;
import org.apache.ibatis.annotations.Param;
import org.springframework.stereotype.Component;
import tk.mybatis.mapper.common.Mapper;
import tk.mybatis.mapper.common.ids.DeleteByIdsMapper;
import tk.mybatis.mapper.common.ids.SelectByIdsMapper;

/**
 *
 * @author xiaodong.wang
 * @email wangxiaodong@beyondcent.com
 * @date 2023/10/5 17:13
 * @since x.x.x
 */
@Component
public interface ArtifactSyncRecordMapper extends SelectByIdsMapper<ArtifactSyncRecord>, Mapper<ArtifactSyncRecord>, DeleteByIdsMapper<ArtifactSyncRecord>{
    
    
    boolean updateStatusAndFailedReasonBySyncNo(@Param("status") Integer status, @Param("failedReason") String failedReason, @Param("syncNo") String syncNo);
}
