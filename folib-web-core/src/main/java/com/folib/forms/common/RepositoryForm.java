package com.folib.forms.common;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import javax.validation.constraints.NotBlank;

/**
 * @author leipenghui
 * @date 2023/9/27
 **/
@Builder
@Data
@AllArgsConstructor
@NoArgsConstructor
public class RepositoryForm {

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
