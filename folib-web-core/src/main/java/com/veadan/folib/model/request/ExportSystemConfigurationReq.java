package com.veadan.folib.model.request;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

/**
 * @author leipenghui
 * @date 2025/3/28
 **/
@Data
@AllArgsConstructor
@NoArgsConstructor
public class ExportSystemConfigurationReq {

    /**
     * 导出到服务器的路径
     */
    private String path;
}
