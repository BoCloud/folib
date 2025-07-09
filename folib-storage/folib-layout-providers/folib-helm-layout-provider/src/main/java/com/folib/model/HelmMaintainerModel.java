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
package com.folib.model;

import com.google.common.base.Strings;
import javax.annotation.Nonnull;

public class HelmMaintainerModel {
    public String email;

    public String name;

    public String url;

    public HelmMaintainerModel() {}

    public HelmMaintainerModel(@Nonnull String name, String email, String url) {
        this.name = name;
        this.email = email;
        this.url = url;
    }

    public String toString() {
        return this.name + this.name + (Strings.isNullOrEmpty(this.email) ? "" : (" - " + this.email));
    }

    public boolean equals(Object o) {
        if (this == o)
            return true;
        if (!(o instanceof HelmMaintainerModel))
            return false;
        HelmMaintainerModel that = (HelmMaintainerModel)o;
        return (((this.name != null) ? this.name.equals(that.name) : (that.name == null)) && ((this.email != null) ? this.email
                .equals(that.email) : (that.email == null)) && ((this.url != null) ? this.url
                .equals(that.url) : (that.url == null)));
    }

    public int hashCode() {
        int result = (this.name != null) ? this.name.hashCode() : 0;
        result = 31 * result + ((this.email != null) ? this.email.hashCode() : 0);
        result = 31 * result + ((this.url != null) ? this.url.hashCode() : 0);
        return result;
    }
}