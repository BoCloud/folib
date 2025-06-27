package com.veadan.folib.php;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * @author veadan
 * @date 2022/12/14
 **/
@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class PhpSearchPackage {

    /**
     * 包名称
     */
    private String name;
    /**
     * 描述
     */
    private String description;
    /**
     * url
     */
    private String url;
    /**
     * 代码仓库
     */
    private String repository;
    /**
     * 下载次数
     */
    private Integer downloads;
    /**
     * favers
     */
    private String favers;
}
