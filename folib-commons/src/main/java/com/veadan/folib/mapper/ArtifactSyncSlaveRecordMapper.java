package com.veadan.folib.mapper;

import com.veadan.folib.entity.ArtifactSyncSlaveRecord;
import org.springframework.stereotype.Component;
import tk.mybatis.mapper.common.Mapper;
import tk.mybatis.mapper.common.ids.DeleteByIdsMapper;
import tk.mybatis.mapper.common.ids.SelectByIdsMapper;

/**
 *
 * @author xiaodong.wang
 * @email wangxiaodong@beyondcent.com
 * @date 2023/12/1 16:14
 * @since x.x.x
 */
@Component
public interface ArtifactSyncSlaveRecordMapper extends SelectByIdsMapper<ArtifactSyncSlaveRecord>, Mapper<ArtifactSyncSlaveRecord>, DeleteByIdsMapper<ArtifactSyncSlaveRecord> {
}
