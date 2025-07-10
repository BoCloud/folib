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
package com.folib.data.service.support.search;

import javax.annotation.concurrent.Immutable;
import java.util.Arrays;
import java.util.List;

import org.springframework.util.StringUtils;

/**
 * @author veadan
 */
@Immutable
public class Sort
{

    public static final Direction DEFAULT_DIRECTION = Direction.ASC;

    private static final Sort DEFAULT_BY_UUID = Sort.by(Order.asc("uuid"));

    private final List<Order> orders;

    private Sort(List<Order> orders)
    {
        this.orders = orders;
    }

    public static Sort byUuid()
    {
        return DEFAULT_BY_UUID;
    }

    public static Sort by(List<Order> orders)
    {
        return new Sort(orders);
    }

    public static Sort by(Order... orders)
    {
        return new Sort(Arrays.asList(orders));
    }

    @Override
    public String toString()
    {
        return StringUtils.collectionToCommaDelimitedString(orders);
    }

    public enum Direction
    {

        ASC, DESC;

    }

    public static class Order
    {

        private final Direction direction;
        private final String property;

        private Order(Direction direction,
                      String property)
        {
            this.direction = direction == null ? DEFAULT_DIRECTION : direction;
            this.property = property;
        }

        public static Order by(String property)
        {
            return new Order(DEFAULT_DIRECTION, property);
        }

        public static Order asc(String property)
        {
            return new Order(Direction.ASC, property);
        }

        public static Order desc(String property)
        {
            return new Order(Direction.DESC, property);
        }

        @Override
        public String toString()
        {
            return String.format("%s %s", property, direction);
        }
    }
}
