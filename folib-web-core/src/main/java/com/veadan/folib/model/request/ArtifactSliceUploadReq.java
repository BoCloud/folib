package com.veadan.folib.model.request;

import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import org.springframework.web.multipart.MultipartFile;

import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.NotNull;
import java.util.Map;

/**
 * @author xiaodong.wang
 * @email wangxiaodong@beyondcent.com
 * @date 2023/11/22 14:39
 * @since x.x.x
 */
@Data
public class ArtifactSliceUploadReq {
    @ApiModelProperty("存储ID")
    @NotEmpty(message = "存储ID不能为空")
    private String storageId;
    @ApiModelProperty("仓库ID")
    @NotEmpty(message = "仓库ID不能为空")
    private String repositoryId;
    @ApiModelProperty("Folib存储路径（不能以`/`开头）")
    @NotEmpty(message = "Folib存储路径不能为空")
    private String path;
    @ApiModelProperty("切片文件")
    @NotNull(message = "切片文件不能为空")
    private MultipartFile file;
    @ApiModelProperty("切片文件合并ID")
    @NotEmpty(message = "切片文件合并ID不能为空")
    private String mergeId;
    @ApiModelProperty("切片当前块数")
    @NotNull(message = "切片当前块数不能为空")
    private Integer chunkIndex;
    @ApiModelProperty("切片最大块数")
    @NotNull(message = "切片最大块数不能为空")
    private Integer chunkIndexMax;
    @ApiModelProperty("切片文件原始MD5（非切片文件的MD5）")
    @NotEmpty(message = "切片文件原始MD5不能为空")
    private String originFileMd5;
    @ApiModelProperty("切片文件MD5")
    private String sliceMd5;
    @ApiModelProperty("制品源数据")
    private Map<String, Object> metaData;
}
