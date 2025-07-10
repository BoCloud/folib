/*
 * Folib - [新一代AI制品仓库]
 * Copyright (C) 2025 bocloud.com.cn <folib@beyondcent.com>
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * 本程序是自由软件：您可依据GNU通用公共许可证（GPL-3.0+）条款重新发布和修改，
 * 但禁止任何形式的商业售卖行为（包括但不限于：直接销售、捆绑销售、云服务商用）。
 *
 * This program is distributed WITHOUT ANY WARRANTY.
 * Commercial sale of this software is expressly prohibited.
 *
 * For license details, see: https://www.gnu.org/licenses/gpl-3.0.html
 * 商业授权咨询请联系：folib@beyondcent.com
 */
package com.folib.domain;

import lombok.Data;
import lombok.NoArgsConstructor;

import javax.annotation.Nonnull;
import java.io.Serializable;
import java.util.List;

/**
 * @author veadan
 **/
@Data
@NoArgsConstructor
public class DebianMetadata implements Serializable, Comparable<DebianMetadata> {

    public String packageName;

    /**
     * 包的版本
     */
    public String version;

    /**
     * 包的目标架构（如 amd64, i386, all）
     */
    public String architecture;

    /**
     * 包的维护者信息
     */
    public String maintainer;


    /**
     * 该包依赖的其他包列表
     */
    public List<String> depends;

    /**
     * 包所属的类别（如 utils, net, admin）
     */
    public String section;

    /**
     * 包的优先级（required, important, standard, optional, extra）
     */
    public String priority;

    /**
     * 包的官方网站
     */
    public String homePage;

    public String website;

    /**
     * 包的详细描述
     */
    public String description;

    /**
     * 包文件在仓库中的路径
     */
    public String filename;


    /**
     * 包文件的 MD5 校验和
     */
    public String md5sum;

    /**
     * 包文件的 SHA1 校验和
     */
    public String sha1;

    /**
     * 包文件的 SHA256 校验和
     */
    public String sha256;

    public String size;

    public String controlFileContent;

    public String license;

    public String source;

    public String artifactRelativePath;


    /**
     * 构造函数
     *
     * @param packageName  包名
     * @param version      版本
     * @param architecture 架构
     */
    public DebianMetadata(String packageName, String version, String architecture) {
        this.packageName = packageName;
        this.version = version;
        this.architecture = architecture;
    }

    /**
     * 添加一个依赖项
     *
     * @param dependency 依赖项
     */
    public void addDepends(String dependency) {
        this.depends.add(dependency);
    }

    public int compareTo(@Nonnull DebianMetadata o) {
        return (this.packageName + "-" + this.architecture + "-" + this.version).compareTo(o.packageName + "-" + o.architecture + "-" + o.version);
    }


}
