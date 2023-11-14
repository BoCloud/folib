package com.veadan.folib.model.request;

import lombok.Data;
import lombok.experimental.Accessors;

import javax.validation.constraints.NotEmpty;

/**
 * @author xiaodong.wang
 * @email wangxiaodong@beyondcent.com
 * @date 2023/11/14 23:50
 * @since x.x.x
 */
@Data
@Accessors(chain = true)
public class ArtifactSliceDownloadInfoReq {
    @NotEmpty(message = "存储ID不能为空")
    private String storageId;
    @NotEmpty(message = "仓库ID不能为空")
    private String repositoryId;
    @NotEmpty(message = "制品路径不能为空")
    private String path;
}
