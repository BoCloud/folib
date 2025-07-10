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

import com.folib.data.criteria.Expression;
import com.folib.data.criteria.Paginator;
import com.folib.data.criteria.Predicate;
import com.folib.data.criteria.Selector;
import com.folib.aql.grammar.AQLParser.QueryContext;
import com.folib.aql.grammar.AQLParser.QueryExpContext;
import com.folib.domain.ArtifactEntity;

/**
 * @author veadan
 *
 */
public class AqlStatementVisitor extends AQLBaseVisitor<Selector<ArtifactEntity>>
{

    private Selector<ArtifactEntity> selector = new Selector<>(ArtifactEntity.class);

    public AqlStatementVisitor()
    {
    }

    @Override
    public Selector<ArtifactEntity> visitQuery(QueryContext ctx)
    {
        Predicate artifactPredicate = Predicate.of(Expression.ExpOperator.IS_NOT_NULL.of("artifactCoordinates"));
        AqlQueryVisitor queryVisitor = new AqlQueryVisitor(artifactPredicate);

        for (QueryExpContext queryExpContext : ctx.queryExp())
        {
            artifactPredicate.and(queryVisitor.visitQueryExp(queryExpContext).nested());
        }
        selector.where(queryVisitor.getRoot());

        AqlPaginatorVisitor aqlPaginatorVisitor = new AqlPaginatorVisitor();
        Paginator paginator = aqlPaginatorVisitor.visitOrderExp(ctx.orderExp());
        paginator = aqlPaginatorVisitor.visitPageExp(ctx.pageExp());

        selector.with(paginator);

        return selector;
    }

}
