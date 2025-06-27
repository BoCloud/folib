package com.veadan.folib.model.request;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import javax.validation.constraints.NotBlank;

/**
 * @author veadan
 * @date 2025/3/28
 **/
@Data
@AllArgsConstructor
@NoArgsConstructor
public class ExportSystemConfigurationReq {

    /**
     * 导出到服务器的路径
     */
    @NotBlank(message = "导出到服务器的路径不能为空")
    private String path;

    /**
     * zip归档
     */
    private Boolean zipArchive;
}
