package com.veadan.folib.domain;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * @author leipenghui
 * @date 2023/9/26
 **/
@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class FolderInfo {

    /**
     * 目录名
     */
    private String name;

    /**
     * 路径
     */
    private String fullPath;

    /**
     * 是否有下级目录 true 有 false 没有
     */
    private boolean hasSubDirectories;

    /**
     * 是否是文件
     */
    private boolean file;
}
