// Generated from com/veadan/folib/nuget/filter/NugetODataFilter.g4 by ANTLR 4.7.1
package com.veadan.folib.nuget.filter;
import org.antlr.v4.runtime.tree.ParseTreeVisitor;

/**
 * This interface defines a complete generic visitor for a parse tree produced
 * by {@link NugetODataFilterParser}.
 *
 * @param <T> The return type of the visit operation. Use {@link Void} for
 * operations with no return type.
 */
public interface NugetODataFilterVisitor<T> extends ParseTreeVisitor<T> {
	/**
	 * Visit a parse tree produced by {@link NugetODataFilterParser#filter}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitFilter(NugetODataFilterParser.FilterContext ctx);
	/**
	 * Visit a parse tree produced by {@link NugetODataFilterParser#filterExp}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitFilterExp(NugetODataFilterParser.FilterExpContext ctx);
	/**
	 * Visit a parse tree produced by {@link NugetODataFilterParser#tokenExp}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitTokenExp(NugetODataFilterParser.TokenExpContext ctx);
	/**
	 * Visit a parse tree produced by {@link NugetODataFilterParser#tokenExpRight}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitTokenExpRight(NugetODataFilterParser.TokenExpRightContext ctx);
	/**
	 * Visit a parse tree produced by {@link NugetODataFilterParser#tokenExpLeft}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitTokenExpLeft(NugetODataFilterParser.TokenExpLeftContext ctx);
	/**
	 * Visit a parse tree produced by {@link NugetODataFilterParser#tokenExpFunction}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitTokenExpFunction(NugetODataFilterParser.TokenExpFunctionContext ctx);
	/**
	 * Visit a parse tree produced by {@link NugetODataFilterParser#fuctionExp}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitFuctionExp(NugetODataFilterParser.FuctionExpContext ctx);
	/**
	 * Visit a parse tree produced by {@link NugetODataFilterParser#filterOp}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitFilterOp(NugetODataFilterParser.FilterOpContext ctx);
	/**
	 * Visit a parse tree produced by {@link NugetODataFilterParser#logicalOp}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitLogicalOp(NugetODataFilterParser.LogicalOpContext ctx);
}