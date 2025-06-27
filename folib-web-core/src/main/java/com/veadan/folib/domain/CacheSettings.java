package com.veadan.folib.domain;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * @author veadan
 * @date 2023/10/24
 **/
@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class CacheSettings {

    /**
     * 启用缓存 true 是 false 否
     */
    private boolean enabled;

    /**
     * 缓存目录
     */
    private String directoryPath;

    /**
     * 最小缓存值
     */
    private String minSize;

    /**
     * 最小缓存值的单位
     */
    private String minSizeUnit;

    /**
     * 最大缓存值
     */
    private String maxSize;

    /**
     * 最大缓存值的单位
     */
    private String maxSizeUnit;

    /**
     * 缓存容量
     */
    private String size;

    /**
     * 缓存容量单位 GB TB
     */
    private String sizeUnit;

    /**
     * 清理条件
     */
    private int clearCondition;

    /**
     * 清理比例
     */
    private int clearProportion;
}
