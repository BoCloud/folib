// Generated from com/veadan/folib/nuget/filter/NugetODataFilter.g4 by ANTLR 4.7.1
package com.veadan.folib.nuget.filter;
import org.antlr.v4.runtime.tree.ParseTreeListener;

/**
 * This interface defines a complete listener for a parse tree produced by
 * {@link NugetODataFilterParser}.
 */
public interface NugetODataFilterListener extends ParseTreeListener {
	/**
	 * Enter a parse tree produced by {@link NugetODataFilterParser#filter}.
	 * @param ctx the parse tree
	 */
	void enterFilter(NugetODataFilterParser.FilterContext ctx);
	/**
	 * Exit a parse tree produced by {@link NugetODataFilterParser#filter}.
	 * @param ctx the parse tree
	 */
	void exitFilter(NugetODataFilterParser.FilterContext ctx);
	/**
	 * Enter a parse tree produced by {@link NugetODataFilterParser#filterExp}.
	 * @param ctx the parse tree
	 */
	void enterFilterExp(NugetODataFilterParser.FilterExpContext ctx);
	/**
	 * Exit a parse tree produced by {@link NugetODataFilterParser#filterExp}.
	 * @param ctx the parse tree
	 */
	void exitFilterExp(NugetODataFilterParser.FilterExpContext ctx);
	/**
	 * Enter a parse tree produced by {@link NugetODataFilterParser#tokenExp}.
	 * @param ctx the parse tree
	 */
	void enterTokenExp(NugetODataFilterParser.TokenExpContext ctx);
	/**
	 * Exit a parse tree produced by {@link NugetODataFilterParser#tokenExp}.
	 * @param ctx the parse tree
	 */
	void exitTokenExp(NugetODataFilterParser.TokenExpContext ctx);
	/**
	 * Enter a parse tree produced by {@link NugetODataFilterParser#tokenExpRight}.
	 * @param ctx the parse tree
	 */
	void enterTokenExpRight(NugetODataFilterParser.TokenExpRightContext ctx);
	/**
	 * Exit a parse tree produced by {@link NugetODataFilterParser#tokenExpRight}.
	 * @param ctx the parse tree
	 */
	void exitTokenExpRight(NugetODataFilterParser.TokenExpRightContext ctx);
	/**
	 * Enter a parse tree produced by {@link NugetODataFilterParser#tokenExpLeft}.
	 * @param ctx the parse tree
	 */
	void enterTokenExpLeft(NugetODataFilterParser.TokenExpLeftContext ctx);
	/**
	 * Exit a parse tree produced by {@link NugetODataFilterParser#tokenExpLeft}.
	 * @param ctx the parse tree
	 */
	void exitTokenExpLeft(NugetODataFilterParser.TokenExpLeftContext ctx);
	/**
	 * Enter a parse tree produced by {@link NugetODataFilterParser#tokenExpFunction}.
	 * @param ctx the parse tree
	 */
	void enterTokenExpFunction(NugetODataFilterParser.TokenExpFunctionContext ctx);
	/**
	 * Exit a parse tree produced by {@link NugetODataFilterParser#tokenExpFunction}.
	 * @param ctx the parse tree
	 */
	void exitTokenExpFunction(NugetODataFilterParser.TokenExpFunctionContext ctx);
	/**
	 * Enter a parse tree produced by {@link NugetODataFilterParser#fuctionExp}.
	 * @param ctx the parse tree
	 */
	void enterFuctionExp(NugetODataFilterParser.FuctionExpContext ctx);
	/**
	 * Exit a parse tree produced by {@link NugetODataFilterParser#fuctionExp}.
	 * @param ctx the parse tree
	 */
	void exitFuctionExp(NugetODataFilterParser.FuctionExpContext ctx);
	/**
	 * Enter a parse tree produced by {@link NugetODataFilterParser#filterOp}.
	 * @param ctx the parse tree
	 */
	void enterFilterOp(NugetODataFilterParser.FilterOpContext ctx);
	/**
	 * Exit a parse tree produced by {@link NugetODataFilterParser#filterOp}.
	 * @param ctx the parse tree
	 */
	void exitFilterOp(NugetODataFilterParser.FilterOpContext ctx);
	/**
	 * Enter a parse tree produced by {@link NugetODataFilterParser#logicalOp}.
	 * @param ctx the parse tree
	 */
	void enterLogicalOp(NugetODataFilterParser.LogicalOpContext ctx);
	/**
	 * Exit a parse tree produced by {@link NugetODataFilterParser#logicalOp}.
	 * @param ctx the parse tree
	 */
	void exitLogicalOp(NugetODataFilterParser.LogicalOpContext ctx);
}