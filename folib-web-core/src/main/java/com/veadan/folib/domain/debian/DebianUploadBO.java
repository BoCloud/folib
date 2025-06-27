package com.veadan.folib.domain.debian;

import lombok.Data;

import javax.validation.constraints.NotBlank;

/**
 * @author veadan
 * @since 2024-09-06 16:33
 */
@Data
public class DebianUploadBO {

    @NotBlank(message = "存储不能为空")
    private String storageId;

    @NotBlank(message = "仓库不能为空")
    private String repositoryId;

    @NotBlank(message = "distribution不能为空")
    private String distribution;

    @NotBlank(message = "component不能为空")
    private String component;

    @NotBlank(message = "architecture不能为空")
    private String architecture;

    @NotBlank(message = "路径不能为空")
    private String path;

    private String version;

    private String fileName;

}
