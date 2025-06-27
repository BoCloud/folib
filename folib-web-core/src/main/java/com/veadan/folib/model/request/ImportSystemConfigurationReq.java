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
public class ImportSystemConfigurationReq {

    /**
     * 导入到服务器的路径或者zip文件
     */
    @NotBlank(message = "导入到服务器的路径或者zip文件不能为空")
    private String path;
}
