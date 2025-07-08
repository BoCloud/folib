package com.folib.model.request;

import io.swagger.annotations.ApiModel;
import lombok.Data;

/**
 * @author veadan
 * @date 2023/12/6 14:11
 */
@Data
@ApiModel("制品晋级/分发记录分页-请求模型")
public class ArtifactSyncRecordPageReq {
    private String storageId;
    private String repositoryId;
    private Integer pageNumber;
    private Integer pageSize;
}
