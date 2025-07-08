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