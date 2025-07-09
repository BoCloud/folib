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
import org.antlr.v4.runtime.tree.ParseTreeVisitor;

/**
 * This interface defines a complete generic visitor for a parse tree produced
 * by {@link AQLParser}.
 *
 * @param <T> The return type of the visit operation. Use {@link Void} for
 * operations with no return type.
 */
public interface AQLVisitor<T> extends ParseTreeVisitor<T> {
	/**
	 * Visit a parse tree produced by {@link AQLParser#query}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitQuery(AQLParser.QueryContext ctx);
	/**
	 * Visit a parse tree produced by {@link AQLParser#pageExp}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitPageExp(AQLParser.PageExpContext ctx);
	/**
	 * Visit a parse tree produced by {@link AQLParser#orderExp}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitOrderExp(AQLParser.OrderExpContext ctx);
	/**
	 * Visit a parse tree produced by {@link AQLParser#orderValue}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitOrderValue(AQLParser.OrderValueContext ctx);
	/**
	 * Visit a parse tree produced by {@link AQLParser#orderDirection}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitOrderDirection(AQLParser.OrderDirectionContext ctx);
	/**
	 * Visit a parse tree produced by {@link AQLParser#queryExp}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitQueryExp(AQLParser.QueryExpContext ctx);
	/**
	 * Visit a parse tree produced by {@link AQLParser#tokenExp}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitTokenExp(AQLParser.TokenExpContext ctx);
	/**
	 * Visit a parse tree produced by {@link AQLParser#tokenPrefix}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitTokenPrefix(AQLParser.TokenPrefixContext ctx);
	/**
	 * Visit a parse tree produced by {@link AQLParser#tokenKey}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitTokenKey(AQLParser.TokenKeyContext ctx);
	/**
	 * Visit a parse tree produced by {@link AQLParser#layoutValue}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitLayoutValue(AQLParser.LayoutValueContext ctx);
	/**
	 * Visit a parse tree produced by {@link AQLParser#tokenValue}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitTokenValue(AQLParser.TokenValueContext ctx);
	/**
	 * Visit a parse tree produced by {@link AQLParser#tokenKeyword}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitTokenKeyword(AQLParser.TokenKeywordContext ctx);
	/**
	 * Visit a parse tree produced by {@link AQLParser#layoutCoordinateKeyword}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitLayoutCoordinateKeyword(AQLParser.LayoutCoordinateKeywordContext ctx);
	/**
	 * Visit a parse tree produced by {@link AQLParser#logicalOp}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitLogicalOp(AQLParser.LogicalOpContext ctx);
	/**
	 * Visit a parse tree produced by {@link AQLParser#and}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitAnd(AQLParser.AndContext ctx);
	/**
	 * Visit a parse tree produced by {@link AQLParser#or}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitOr(AQLParser.OrContext ctx);
}