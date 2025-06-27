package com.veadan.folib.controllers.adapter.jfrog.res;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * @author veadan
 * @date 2024/12/23
 **/
@Builder
@Data
@AllArgsConstructor
@NoArgsConstructor
public class BatchDownloadRes {

    /**
     * 下载地址
     */
    private String url;

    /**
     * 文件下载到本地的位置
     */
    private String target;
}
