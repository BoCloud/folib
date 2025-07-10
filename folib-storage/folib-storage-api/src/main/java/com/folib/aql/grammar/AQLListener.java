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
// Generated from com/veadan/folib/aql/grammar/AQL.g4 by ANTLR 4.7.1
package com.folib.aql.grammar;
import org.antlr.v4.runtime.tree.ParseTreeListener;

/**
 * This interface defines a complete listener for a parse tree produced by
 * {@link AQLParser}.
 */
public interface AQLListener extends ParseTreeListener {
    /**
     * Enter a parse tree produced by {@link AQLParser#query}.
     * @param ctx the parse tree
     */
    void enterQuery(AQLParser.QueryContext ctx);
    /**
     * Exit a parse tree produced by {@link AQLParser#query}.
     * @param ctx the parse tree
     */
    void exitQuery(AQLParser.QueryContext ctx);
    /**
     * Enter a parse tree produced by {@link AQLParser#pageExp}.
     * @param ctx the parse tree
     */
    void enterPageExp(AQLParser.PageExpContext ctx);
    /**
     * Exit a parse tree produced by {@link AQLParser#pageExp}.
     * @param ctx the parse tree
     */
    void exitPageExp(AQLParser.PageExpContext ctx);
    /**
     * Enter a parse tree produced by {@link AQLParser#orderExp}.
     * @param ctx the parse tree
     */
    void enterOrderExp(AQLParser.OrderExpContext ctx);
    /**
     * Exit a parse tree produced by {@link AQLParser#orderExp}.
     * @param ctx the parse tree
     */
    void exitOrderExp(AQLParser.OrderExpContext ctx);
    /**
     * Enter a parse tree produced by {@link AQLParser#orderValue}.
     * @param ctx the parse tree
     */
    void enterOrderValue(AQLParser.OrderValueContext ctx);
    /**
     * Exit a parse tree produced by {@link AQLParser#orderValue}.
     * @param ctx the parse tree
     */
    void exitOrderValue(AQLParser.OrderValueContext ctx);
    /**
     * Enter a parse tree produced by {@link AQLParser#orderDirection}.
     * @param ctx the parse tree
     */
    void enterOrderDirection(AQLParser.OrderDirectionContext ctx);
    /**
     * Exit a parse tree produced by {@link AQLParser#orderDirection}.
     * @param ctx the parse tree
     */
    void exitOrderDirection(AQLParser.OrderDirectionContext ctx);
    /**
     * Enter a parse tree produced by {@link AQLParser#queryExp}.
     * @param ctx the parse tree
     */
    void enterQueryExp(AQLParser.QueryExpContext ctx);
    /**
     * Exit a parse tree produced by {@link AQLParser#queryExp}.
     * @param ctx the parse tree
     */
    void exitQueryExp(AQLParser.QueryExpContext ctx);
    /**
     * Enter a parse tree produced by {@link AQLParser#tokenExp}.
     * @param ctx the parse tree
     */
    void enterTokenExp(AQLParser.TokenExpContext ctx);
    /**
     * Exit a parse tree produced by {@link AQLParser#tokenExp}.
     * @param ctx the parse tree
     */
    void exitTokenExp(AQLParser.TokenExpContext ctx);
    /**
     * Enter a parse tree produced by {@link AQLParser#tokenPrefix}.
     * @param ctx the parse tree
     */
    void enterTokenPrefix(AQLParser.TokenPrefixContext ctx);
    /**
     * Exit a parse tree produced by {@link AQLParser#tokenPrefix}.
     * @param ctx the parse tree
     */
    void exitTokenPrefix(AQLParser.TokenPrefixContext ctx);
    /**
     * Enter a parse tree produced by {@link AQLParser#tokenKey}.
     * @param ctx the parse tree
     */
    void enterTokenKey(AQLParser.TokenKeyContext ctx);
    /**
     * Exit a parse tree produced by {@link AQLParser#tokenKey}.
     * @param ctx the parse tree
     */
    void exitTokenKey(AQLParser.TokenKeyContext ctx);
    /**
     * Enter a parse tree produced by {@link AQLParser#layoutValue}.
     * @param ctx the parse tree
     */
    void enterLayoutValue(AQLParser.LayoutValueContext ctx);
    /**
     * Exit a parse tree produced by {@link AQLParser#layoutValue}.
     * @param ctx the parse tree
     */
    void exitLayoutValue(AQLParser.LayoutValueContext ctx);
    /**
     * Enter a parse tree produced by {@link AQLParser#tokenValue}.
     * @param ctx the parse tree
     */
    void enterTokenValue(AQLParser.TokenValueContext ctx);
    /**
     * Exit a parse tree produced by {@link AQLParser#tokenValue}.
     * @param ctx the parse tree
     */
    void exitTokenValue(AQLParser.TokenValueContext ctx);
    /**
     * Enter a parse tree produced by {@link AQLParser#tokenKeyword}.
     * @param ctx the parse tree
     */
    void enterTokenKeyword(AQLParser.TokenKeywordContext ctx);
    /**
     * Exit a parse tree produced by {@link AQLParser#tokenKeyword}.
     * @param ctx the parse tree
     */
    void exitTokenKeyword(AQLParser.TokenKeywordContext ctx);
    /**
     * Enter a parse tree produced by {@link AQLParser#layoutCoordinateKeyword}.
     * @param ctx the parse tree
     */
    void enterLayoutCoordinateKeyword(AQLParser.LayoutCoordinateKeywordContext ctx);
    /**
     * Exit a parse tree produced by {@link AQLParser#layoutCoordinateKeyword}.
     * @param ctx the parse tree
     */
    void exitLayoutCoordinateKeyword(AQLParser.LayoutCoordinateKeywordContext ctx);
    /**
     * Enter a parse tree produced by {@link AQLParser#logicalOp}.
     * @param ctx the parse tree
     */
    void enterLogicalOp(AQLParser.LogicalOpContext ctx);
    /**
     * Exit a parse tree produced by {@link AQLParser#logicalOp}.
     * @param ctx the parse tree
     */
    void exitLogicalOp(AQLParser.LogicalOpContext ctx);
    /**
     * Enter a parse tree produced by {@link AQLParser#and}.
     * @param ctx the parse tree
     */
    void enterAnd(AQLParser.AndContext ctx);
    /**
     * Exit a parse tree produced by {@link AQLParser#and}.
     * @param ctx the parse tree
     */
    void exitAnd(AQLParser.AndContext ctx);
    /**
     * Enter a parse tree produced by {@link AQLParser#or}.
     * @param ctx the parse tree
     */
    void enterOr(AQLParser.OrContext ctx);
    /**
     * Exit a parse tree produced by {@link AQLParser#or}.
     * @param ctx the parse tree
     */
    void exitOr(AQLParser.OrContext ctx);
}