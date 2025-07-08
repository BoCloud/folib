package com.folib.vo;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import javax.validation.constraints.NotBlank;

/**
 * @author veadan
 * @date 2024/12/23
 **/
@Data
@AllArgsConstructor
@NoArgsConstructor
public class ResolvePathFiles {

    /**
     * glob路径匹配模式
     */
    @NotBlank(message = "请传入pattern参数")
    private String pattern;

    /**
     * 文件下载到本地的位置
     */
    @NotBlank(message = "请传入target参数")
    private String target;

    /**
     * 是否递归子目录，true递归子目录，false不递归子目录，默认为true
     */
    private Boolean recursive = true;

    /**
     * 是否创建子目录，true创建子目录，false不创建子目录，默认为false
     */
    private Boolean flat = false;

}
