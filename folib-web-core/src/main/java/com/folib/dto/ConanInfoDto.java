package com.folib.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import javax.validation.constraints.NotBlank;

/**
 * conanInfo
 *
 * @author veadan
 */
@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class ConanInfoDto {
    /**
     * 存储空间
     */
    @NotBlank(message = "存储空间不能为空")
    private String storageId;

    /**
     * 所属仓库
     */
    @NotBlank(message = "所属仓库不能为空")
    private String repositoryId;
    /**
     * 路径
     */
    @NotBlank(message = "路径不能为空")
    private String artifactPath;
}
