package com.veadan.folib.scanner.entity;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;

/**
 * @author leipenghui
 * @date 2022/9/8
 **/
@Data
@Accessors(chain = true)
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class SeverityVO implements Serializable {

    /**
     * 制品路径
     */
    private String path;

    /**
     * 存储空间
     */
    private String storage;

    /**
     * 仓库名称
     */
    private String repository;

    /**
     * 报告
     */
    private String report;

    /**
     * 严重的数量
     */
    private Long critical;

    /**
     * 高危的数量
     */
    private Long high;

    /**
     * 中危的数量
     */
    private Long medium;

    /**
     * 低危的数量
     */
    private Long low;
    /**
     * 漏洞数量
     */
    private Integer vulnerabilitesCount;
    /**
     * 前端是否展示
     */
    private Boolean show;
}
