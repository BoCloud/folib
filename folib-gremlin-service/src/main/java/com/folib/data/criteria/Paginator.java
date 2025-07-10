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
package com.folib.data.criteria;

public class Paginator
{

    public static final Integer MAX_LIMIT = 9999;

    private Long skip;
    private Integer limit;

    private String property;
    private Order order = Order.ASC;

    private Boolean useLimit;

    public Long getSkip()
    {
        return skip == null ? Integer.valueOf(0) : skip;
    }

    public void setSkip(Long skip)
    {
        this.skip = skip;
    }

    public Integer getLimit()
    {
        return limit == null || limit < 0 || limit > MAX_LIMIT ? MAX_LIMIT : limit;
    }

    public void setLimit(Integer limit)
    {
        this.limit = limit;
    }

    public String getProperty()
    {
        return property;
    }

    public void setProperty(String orderBy)
    {
        this.property = orderBy;
    }

    public Order getOrder()
    {
        return order;
    }

    public void setOrder(Order order)
    {
        this.order = order;
    }

    public static enum Order
    {
        ASC, DESC;
    }

    public Boolean getUseLimit() {
        return useLimit;
    }

    public void setUseLimit(Boolean useLimit) {
        this.useLimit = useLimit;
    }
}
