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
package com.folib.nuget.filter;

import com.folib.artifact.criteria.ArtifactEntryCriteria;
import com.folib.data.criteria.Expression;
import com.folib.data.criteria.Predicate;
import com.folib.data.criteria.Selector;
import org.apache.commons.lang3.StringUtils;
import com.folib.artifact.ArtifactTag;

/**
 * This class purpose is to construct {@link Predicate} instance which can be
 * used with {@link Selector} to perform Database queries.
 * 
 * Every method here should produce {@link Predicate} instance according to
 * filter expression it visit.
 * 
 * @author veadan
 *
 */
public class NugetODataQueryVisitor extends NugetODataFilterBaseVisitor<Predicate>
{

    /**
     * The Root predicate in the parse tree.
     */
    private Predicate root = Predicate.empty();

    public NugetODataQueryVisitor()
    {
        super();
    }

    @Override
    public Predicate visitFilter(NugetODataFilterParser.FilterContext ctx)
    {
        Predicate p = super.visitFilter(ctx);
        return root.and(p);
    }

    @Override
    public Predicate visitFilterExp(NugetODataFilterParser.FilterExpContext ctx)
    {
        if (ctx.tokenExp() != null)
        {
            return visitTokenExp(ctx.tokenExp());
        }
        else if (ctx.vNestedFilterExp != null)
        {
            return visitFilterExp(ctx.vNestedFilterExp);
        }

        Predicate.BooleanOperator booleanOperator = Predicate.BooleanOperator.valueOf(ctx.vLogicalOp.getText().toUpperCase());

        Predicate filterExpRoot = Predicate.empty();

        Predicate p1 = visitFilterExp(ctx.vFilterExpLeft);
        Predicate p2 = visitFilterExp(ctx.vFilterExpRight);

        if (Predicate.BooleanOperator.AND.equals(booleanOperator))
        {
            filterExpRoot.and(p1).and(p2);
        }
        else if (Predicate.BooleanOperator.OR.equals(booleanOperator))
        {
            filterExpRoot.or(p1).or(p2);
        }

        return filterExpRoot;
    }

    @Override
    public Predicate visitTokenExp(NugetODataFilterParser.TokenExpContext ctx)
    {
        if (ctx.TAG() != null)
        {
            ArtifactEntryCriteria c = new ArtifactEntryCriteria();
            c.getTagSet().add(ArtifactTag.LAST_VERSION);

            return Predicate.of(Expression.ExpOperator.CONTAINS.of("tagSet.name", ArtifactTag.LAST_VERSION));
        }

        Predicate p = visitTokenExpLeft(ctx.vTokenExpLeft);

        String attributeValue = ctx.vTokenExpRight.getText();
        attributeValue = StringUtils.unwrap(attributeValue, "'");
        p.getExpression().setValue(attributeValue);

        return p;
    }

    @Override
    public Predicate visitTokenExpLeft(NugetODataFilterParser.TokenExpLeftContext ctx)
    {
        if (ctx.ATTRIBUTE() != null)
        {
            String attribute = ctx.ATTRIBUTE().getText();
            return Predicate.of(Expression.ExpOperator.EQ.of(String.format("artifactCoordinates.coordinates.%s",
                                                                attribute.toLowerCase()),
                                                  null));
        }
        return visitTokenExpFunction(ctx.tokenExpFunction());
    }

    @Override
    public Predicate visitTokenExpFunction(NugetODataFilterParser.TokenExpFunctionContext ctx)
    {
        String attribute = ctx.ATTRIBUTE().getText().toLowerCase();

        if (ctx.fuctionExp().TO_LOWER() != null)
        {
            attribute = String.format("%s.toLowerCase()", attribute);
        }

        return Predicate.of(Expression.ExpOperator.EQ.of(String.format("artifactCoordinates.coordinates.%s",
                                                            attribute),
                                              null));
    }

}
