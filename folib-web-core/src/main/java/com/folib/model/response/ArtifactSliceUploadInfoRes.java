package com.folib.model.response;

import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

/**
 * @author veadan
 * @date 2023/11/22 14:39
 */
@Data
public class ArtifactSliceUploadInfoRes {
    @ApiModelProperty("切片文件合并ID")
    private String mergeId;
    @ApiModelProperty("目标上传节点设置的切片大小（MB）上限2000MB=2048000KB")
    private Integer chunkSize;
}
