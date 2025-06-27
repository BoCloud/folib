package com.veadan.folib.dto.common;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import javax.validation.constraints.NotBlank;

/**
 * @author veadan
 * @date 2023/9/27
 **/
@Builder
@Data
@AllArgsConstructor
@NoArgsConstructor
public class RepositoryDto {

    /**
     * 存储空间
     */
    @NotBlank(message = "存储空间不能为空")
    private String storageId;

    /**
     * 仓库
     */
    @NotBlank(message = "存储空间不能为空")
    private String repositoryId;
}
