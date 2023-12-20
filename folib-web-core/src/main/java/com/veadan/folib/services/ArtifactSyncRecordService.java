package com.veadan.folib.services;

import com.veadan.folib.model.request.ArtifactSyncRecordPageReq;
import com.veadan.folib.model.response.ArtifactSyncRecordPageRes;
import com.veadan.folib.scanner.common.msg.TableResultResponse;

/**
 * @author xiaodong.wang
 * @email wangxiaodong@beyondcent.com
 * @date 2023/12/6 14:10
 * @since x.x.x
 */
public interface ArtifactSyncRecordService {
    TableResultResponse<ArtifactSyncRecordPageRes> page(ArtifactSyncRecordPageReq model);
}
