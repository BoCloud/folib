package com.veadan.folib.vo;

import com.alibaba.fastjson.JSONObject;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;

/**
 * @author veadan
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
    private JSONObject config;
    /**
     * 容器
     */
    private String container;
    /**
     * 容器配置信息
     */
    private JSONObject containerConfig;
    /**
     * 制作Docker版本
     */
    private String dockerVersion;
    /**
     * 制作历史
     */
    private List<DockHistory> history;
    /**
     * 文件
     */
    private DockerRootFs rootFs;
}

