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
package com.folib.aql.grammar;

import java.util.Optional;

import com.folib.data.criteria.Paginator;
import com.folib.aql.grammar.AQLParser.OrderExpContext;
import com.folib.aql.grammar.AQLParser.PageExpContext;

/**
 * @author veadan
 *
 */
public class AqlPaginatorVisitor extends AQLBaseVisitor<Paginator>
{

    private Paginator paginator = new Paginator();

    public AqlPaginatorVisitor()
    {
        super();
        paginator.setLimit(25);
    }

    @Override
    public Paginator visitPageExp(PageExpContext ctx)
    {
        if (ctx == null)
        {
            return paginator;
        }
        paginator.setSkip(Long.valueOf(ctx.NUMBER().getText()));

        return paginator;
    }

    @Override
    public Paginator visitOrderExp(OrderExpContext ctx)
    {
        if (ctx == null)
        {
            return paginator;
        }

        if (Paginator.Order.DESC.toString().equalsIgnoreCase(ctx.orderDirection().getText()))
        {
            paginator.setOrder(Paginator.Order.DESC);
        }

        String aqlOrderProperty = ctx.orderValue().getText();

        for (AqlMapping aqlPropertyKeyword : AqlMapping.values())
        {
            if (!aqlPropertyKeyword.toString().equalsIgnoreCase(aqlOrderProperty))
            {
                continue;
            }

            paginator.setProperty(aqlPropertyKeyword.property());

            break;
        }

        paginator.setProperty(Optional.ofNullable(paginator.getProperty())
                                      .orElse(String.format("artifactCoordinates.coordinates.%s",
                                                            aqlOrderProperty)));

        return paginator;
    }

}
