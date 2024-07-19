package com.veadan.folib.mapper;

import ch.qos.logback.core.util.FileSize;
import com.veadan.folib.dto.ArtifactSyncRecordCountDto;
import com.veadan.folib.dto.FileSizeStatisticsDto;
import com.veadan.folib.entity.ArtifactSyncRecord;
import org.apache.ibatis.annotations.Param;
import org.springframework.stereotype.Component;
import tk.mybatis.mapper.common.Mapper;
import tk.mybatis.mapper.common.ids.DeleteByIdsMapper;
import tk.mybatis.mapper.common.ids.SelectByIdsMapper;

import javax.validation.constraints.NotNull;
import java.util.Date;
import java.util.List;

/**
 *
 * @author xiaodong.wang
 * @email wangxiaodong@beyondcent.com
 * @date 2023/10/5 17:13
 * @since x.x.x
 */
@Component
public interface ArtifactSyncRecordMapper extends SelectByIdsMapper<ArtifactSyncRecord>, Mapper<ArtifactSyncRecord>, DeleteByIdsMapper<ArtifactSyncRecord>{
    
    
    boolean updateStatusAndFailedReasonBySyncNo(@Param("status") Integer status, @Param("failedReason") String failedReason, @Param("syncNo") String syncNo, @Param("updateTime") Date updateTime);
    
    List<ArtifactSyncRecord> selectClearRecordList(@Param("storageId") String storageId, @Param("repositoryId") String repositoryId, @Param("time") Date time);

    ArtifactSyncRecord selectBySyncNo(@NotNull @Param("syncNo") String syncNo);

    ArtifactSyncRecordCountDto countArtifactSyncRecord(@Param("days") Integer days);

    List<ArtifactSyncRecordCountDto> countByDateArtifactSyncRecord(@Param("days") Integer days);

   List<FileSizeStatisticsDto> fileSizeStatisticsByWarehouse(@Param("days") Integer days,@Param("limitNumber") Integer limitNumber);
}
