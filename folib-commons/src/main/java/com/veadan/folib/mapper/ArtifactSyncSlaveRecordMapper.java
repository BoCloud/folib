package com.veadan.folib.mapper;

import com.veadan.folib.entity.ArtifactSyncSlaveRecord;
import org.apache.ibatis.annotations.Param;
import org.apache.ibatis.annotations.Update;
import org.springframework.stereotype.Component;
import tk.mybatis.mapper.common.Mapper;
import tk.mybatis.mapper.common.MySqlMapper;
import tk.mybatis.mapper.common.ids.DeleteByIdsMapper;
import tk.mybatis.mapper.common.ids.SelectByIdsMapper;

import java.util.Date;
import java.util.List;

/**
 *
 * @author xiaodong.wang
 * @email wangxiaodong@beyondcent.com
 * @date 2023/12/1 16:14
 * @since x.x.x
 */
@Component
public interface ArtifactSyncSlaveRecordMapper extends SelectByIdsMapper<ArtifactSyncSlaveRecord>, Mapper<ArtifactSyncSlaveRecord>, DeleteByIdsMapper<ArtifactSyncSlaveRecord>, MySqlMapper<ArtifactSyncSlaveRecord> {
    
    @Update("update artifact_sync_slave_record set status = #{status}, update_time = #{updateTime}, failed_reason = #{failedReason} where id = #{id}")
    Boolean updateRecordStatus(@Param("id") Long id, @Param("status") Integer status, @Param("updateTime") Date updateTime, @Param("failedReason") String failedReason);
    
    List<ArtifactSyncSlaveRecord> selectListBySyncNoList(@Param("syncNoList") List<String> syncNoList);
    
    Boolean batchDeleteBySyncNoList(@Param("syncNoList") List<String> syncNoList);

    ArtifactSyncSlaveRecord selectBySyncNo(@Param("syncNo") String syncNo);
}
