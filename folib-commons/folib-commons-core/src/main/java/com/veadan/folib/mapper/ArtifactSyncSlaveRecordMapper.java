package com.veadan.folib.mapper;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.baomidou.mybatisplus.core.mapper.Mapper;
import com.veadan.folib.entity.ArtifactSyncSlaveRecord;
import org.apache.ibatis.annotations.Delete;
import org.apache.ibatis.annotations.Param;
import org.apache.ibatis.annotations.Select;
import org.apache.ibatis.annotations.Update;

import org.springframework.stereotype.Component;

import java.util.Date;
import java.util.List;

/**
 *
 * @author veadan
 * @date 2023/12/1 16:14
 */
@Component
public interface ArtifactSyncSlaveRecordMapper extends BaseMapper<ArtifactSyncSlaveRecord> {
    @Update("update artifact_sync_slave_record set status = #{status}, update_time = #{updateTime}, failed_reason = #{failedReason} where id = #{id}")
    Boolean updateRecordStatus(@Param("id") Long id, @Param("status") Integer status, @Param("updateTime") Date updateTime, @Param("failedReason") String failedReason);
    
    List<ArtifactSyncSlaveRecord> selectListBySyncNoList(@Param("syncNoList") List<String> syncNoList);
    
    Boolean batchDeleteBySyncNoList(@Param("syncNoList") List<String> syncNoList);

    List<ArtifactSyncSlaveRecord> selectBySyncNo(@Param("syncNo") String syncNo);

    ArtifactSyncSlaveRecord  selectBySyncNoAndStatus(@Param("syncNo") String syncNo,@Param("status") Integer status);

    @Select("select sum(file_size) from artifact_sync_slave_record where update_time > date_sub(now(), interval #{days} day)")
    Long  statisticsFileSize(@Param("days") Integer days);

    @Delete("delete from artifact_sync_slave_record where sync_no = #{syncNo}")
    Long deleteBySyncNo(@Param("syncNo") String syncNo);
}
