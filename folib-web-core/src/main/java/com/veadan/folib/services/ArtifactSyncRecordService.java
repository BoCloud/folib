package com.veadan.folib.services;

import com.veadan.folib.model.request.ArtifactSyncRecordPageReq;
import com.veadan.folib.model.response.ArtifactSyncRecordCountRes;
import com.veadan.folib.model.response.ArtifactSyncRecordPageRes;
import com.veadan.folib.model.response.FileSizeStatisticsRes;
import com.veadan.folib.scanner.common.msg.TableResultResponse;

import java.util.List;

/**
 * @author xiaodong.wang
 * @email wangxiaodong@beyondcent.com
 * @date 2023/12/6 14:10
 * @since x.x.x
 */
public interface ArtifactSyncRecordService {
    TableResultResponse<ArtifactSyncRecordPageRes> page(ArtifactSyncRecordPageReq model);

    /**
     * 统计分发晋级 days 天数内 的数量
     * @param days 天数
     * @return ArtifactSyncRecordCountRes
     */
    ArtifactSyncRecordCountRes getCount(Integer days);

    /**
     * 统计分发晋级 days 天数内 成功失败的数量
     * @param days 天数
     * @return ArtifactSyncRecordCountRes
     */
    List<ArtifactSyncRecordCountRes> getStatusTrends(Integer days);

    /**
     * 统计某些天内晋级或分发文件（大小） 排名靠前的仓库
     * @param days 天数
     * @param limitNumber 统计数量
     * @return FileSizeStatisticsRes
     */
    List<FileSizeStatisticsRes> fileSizeStatisticsByWarehouse(Integer days, Integer limitNumber);
}
