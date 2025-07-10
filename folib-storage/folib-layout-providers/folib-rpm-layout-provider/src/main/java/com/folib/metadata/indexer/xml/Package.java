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
package com.folib.metadata.indexer.xml;

import lombok.Getter;
import lombok.Setter;

import java.util.Objects;

@Getter
@Setter
// 软件包对象
public class Package {
    private String type;
    private String name;
    private String arch;
    private Version version;
    private Checksum checksum;
    private String summary;
    private String description;
    private String packager;
    private String url;
    private Time time;
    private Size size;
    private Location location;
    private Format format;

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Package aPackage = (Package) o;
        return Objects.equals(version, aPackage.version) &&
                Objects.equals(name, aPackage.name) &&
                Objects.equals(arch, aPackage.arch);
    }

    @Override
    public int hashCode() {
        return Objects.hash(version, name,arch);
    }
}
