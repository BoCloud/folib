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

import com.folib.domain.ArtifactEntryExpressionBuilder;
import org.antlr.v4.runtime.tree.TerminalNode;
import com.folib.aql.grammar.AQLParser.TokenExpContext;
import com.folib.aql.grammar.AQLParser.TokenKeyContext;
import com.folib.aql.grammar.AQLParser.TokenValueContext;

/**
 * @author veadan
 *
 */
public class AqlExpressionVisitor extends AQLBaseVisitor<ArtifactEntryExpressionBuilder>
{

    private ArtifactEntryExpressionBuilder expressionBuilder = new ArtifactEntryExpressionBuilder(
            new AqlExpressionDialect());

    @Override
    public ArtifactEntryExpressionBuilder visitTokenExp(TokenExpContext ctx)
    {
        
        
        TokenKeyContext tokenKey;
        TerminalNode layout;
        if ((tokenKey = ctx.tokenKey()) != null)
        {
            visitTokenKey(tokenKey);
            visitTokenValue(ctx.tokenValue());
        }
        else if ((layout = ctx.LAYOUT()) != null)
        {
            expressionBuilder = expressionBuilder.of(layout.getText()).with(ctx.layoutValue().getText());
        }
        
        return expressionBuilder;
    }
    
    

    @Override
    public ArtifactEntryExpressionBuilder visitTokenValue(TokenValueContext ctx)
    {
        return expressionBuilder = expressionBuilder.with(ctx.getText());
    }

    @Override
    public ArtifactEntryExpressionBuilder visitTokenKey(TokenKeyContext ctx)
    {
        String attribute = null;
        if (ctx.layoutCoordinateKeyword() != null)
        {
            attribute = ctx.layoutCoordinateKeyword().getText();
        }
        else if (ctx.tokenKeyword() != null)
        {
            attribute = ctx.tokenKeyword().getText();
        }
        return expressionBuilder = expressionBuilder.of(attribute);
    }

    public ArtifactEntryExpressionBuilder getExpressionBuilder()
    {
        return expressionBuilder;
    }

}
