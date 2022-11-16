package com.veadan.folib.vo;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;

/**
 * @author leipenghui
 * @date 2022/11/16
 **/
@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class ManifestConfig {

    /**
     * 基础架构
     */
    private String architecture;
    /**
     * 作者
     */
    private String author;
    /**
     * 创建时间
     */
    private String created;
    /**
     * 镜像OS
     */
    private String os;
    /**
     * 配置信息
     */
    private DockerConfig config;
    /**
     * 制作历史
     */
    private List<DockHistory> history;
    /**
     * 文件
     */
    private DockerRootFs rootFs;
}

