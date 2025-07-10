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

import org.antlr.v4.runtime.*;
import org.antlr.v4.runtime.atn.ATN;
import org.antlr.v4.runtime.atn.ATNDeserializer;
import org.antlr.v4.runtime.atn.ParserATNSimulator;
import org.antlr.v4.runtime.atn.PredictionContextCache;
import org.antlr.v4.runtime.dfa.DFA;
import org.antlr.v4.runtime.tree.ParseTreeListener;
import org.antlr.v4.runtime.tree.ParseTreeVisitor;
import org.antlr.v4.runtime.tree.TerminalNode;

import java.util.List;

@SuppressWarnings({"all", "warnings", "unchecked", "unused", "cast"})
public class AQLParser extends Parser {
	static { RuntimeMetaData.checkVersion("4.7.1", RuntimeMetaData.VERSION); }

	protected static final DFA[] _decisionToDFA;
	protected static final PredictionContextCache _sharedContextCache =
		new PredictionContextCache();
	public static final int
		LAYOUT_GITLFS=1, LAYOUT_MAVEN=2, LAYOUT_RAW=3, LAYOUT_NPM=4, LAYOUT_NUGET=5, 
		LAYOUT_COORDINATE_GITLFS_PATH=6, LAYOUT_COORDINATE_MAVEN_EXTENSION=7, 
		LAYOUT_COORDINATE_MAVEN_GROUPID=8, LAYOUT_COORDINATE_MAVEN_CLASSIFIER=9, 
		LAYOUT_COORDINATE_MAVEN_ARTIFACTID=10, LAYOUT_COORDINATE_RAW_PATH=11, 
		LAYOUT_COORDINATE_NPM_EXTENSION=12, LAYOUT_COORDINATE_NPM_SCOPE=13, LAYOUT_COORDINATE_NPM_NAME=14, 
		LAYOUT_COORDINATE_NPM_DISTRIBUTION=15, LAYOUT_COORDINATE_NUGET_ID=16, 
		LAYOUT_COORDINATE_NUGET_TYPE=17, STORAGE=18, REPOSITORY=19, LAYOUT=20, 
		VERSION=21, TAG=22, FROM=23, TO=24, AGE=25, ASC=26, DESC=27, PAGE_SKIP=28, 
		AMP=29, DOUBLE_AMP=30, AND=31, PIPE=32, DOUBLE_PIPE=33, OR=34, PLUS=35, 
		NEGATION=36, COLON=37, ROUND_BRACKET_LEFT=38, ROUND_BRACKET_RIGHT=39, 
		NUMBER=40, VALUE=41, STRING=42, WHITESPACE=43;
	public static final int
		RULE_query = 0, RULE_pageExp = 1, RULE_orderExp = 2, RULE_orderValue = 3, 
		RULE_orderDirection = 4, RULE_queryExp = 5, RULE_tokenExp = 6, RULE_tokenPrefix = 7, 
		RULE_tokenKey = 8, RULE_layoutValue = 9, RULE_tokenValue = 10, RULE_tokenKeyword = 11, 
		RULE_layoutCoordinateKeyword = 12, RULE_logicalOp = 13, RULE_and = 14, 
		RULE_or = 15;
	public static final String[] ruleNames = {
		"query", "pageExp", "orderExp", "orderValue", "orderDirection", "queryExp", 
		"tokenExp", "tokenPrefix", "tokenKey", "layoutValue", "tokenValue", "tokenKeyword", 
		"layoutCoordinateKeyword", "logicalOp", "and", "or"
	};

	private static final String[] _LITERAL_NAMES = {
		null, "'GitLfs'", "'maven'", "'Raw'", "'npm'", "'nuget'", null, null, 
		"'groupId'", "'classifier'", "'artifactId'", null, null, "'scope'", "'name'", 
		"'distribution'", "'id'", "'type'", "'storage'", "'repository'", "'layout'", 
		"'version'", "'tag'", "'from'", "'to'", "'age'", "'asc'", "'desc'", "'skip'", 
		"'&'", "'&&'", "'and'", "'|'", "'||'", "'or'", "'+'", null, "':'", "'('", 
		"')'"
	};
	private static final String[] _SYMBOLIC_NAMES = {
		null, "LAYOUT_GITLFS", "LAYOUT_MAVEN", "LAYOUT_RAW", "LAYOUT_NPM", "LAYOUT_NUGET", 
		"LAYOUT_COORDINATE_GITLFS_PATH", "LAYOUT_COORDINATE_MAVEN_EXTENSION", 
		"LAYOUT_COORDINATE_MAVEN_GROUPID", "LAYOUT_COORDINATE_MAVEN_CLASSIFIER", 
		"LAYOUT_COORDINATE_MAVEN_ARTIFACTID", "LAYOUT_COORDINATE_RAW_PATH", "LAYOUT_COORDINATE_NPM_EXTENSION", 
		"LAYOUT_COORDINATE_NPM_SCOPE", "LAYOUT_COORDINATE_NPM_NAME", "LAYOUT_COORDINATE_NPM_DISTRIBUTION", 
		"LAYOUT_COORDINATE_NUGET_ID", "LAYOUT_COORDINATE_NUGET_TYPE", "STORAGE", 
		"REPOSITORY", "LAYOUT", "VERSION", "TAG", "FROM", "TO", "AGE", "ASC", 
		"DESC", "PAGE_SKIP", "AMP", "DOUBLE_AMP", "AND", "PIPE", "DOUBLE_PIPE", 
		"OR", "PLUS", "NEGATION", "COLON", "ROUND_BRACKET_LEFT", "ROUND_BRACKET_RIGHT", 
		"NUMBER", "VALUE", "STRING", "WHITESPACE"
	};
	public static final Vocabulary VOCABULARY = new VocabularyImpl(_LITERAL_NAMES, _SYMBOLIC_NAMES);

	/**
	 * @deprecated Use {@link #VOCABULARY} instead.
	 */
	@Deprecated
	public static final String[] tokenNames;
	static {
		tokenNames = new String[_SYMBOLIC_NAMES.length];
		for (int i = 0; i < tokenNames.length; i++) {
			tokenNames[i] = VOCABULARY.getLiteralName(i);
			if (tokenNames[i] == null) {
				tokenNames[i] = VOCABULARY.getSymbolicName(i);
			}

			if (tokenNames[i] == null) {
				tokenNames[i] = "<INVALID>";
			}
		}
	}

	@Override
	@Deprecated
	public String[] getTokenNames() {
		return tokenNames;
	}

	@Override

	public Vocabulary getVocabulary() {
		return VOCABULARY;
	}

	@Override
	public String getGrammarFileName() { return "AQL.g4"; }

	@Override
	public String[] getRuleNames() { return ruleNames; }

	@Override
	public String getSerializedATN() { return _serializedATN; }

	@Override
	public ATN getATN() { return _ATN; }

	public AQLParser(TokenStream input) {
		super(input);
		_interp = new ParserATNSimulator(this,_ATN,_decisionToDFA,_sharedContextCache);
	}
	public static class QueryContext extends ParserRuleContext {
		public TerminalNode EOF() { return getToken(AQLParser.EOF, 0); }
		public List<QueryExpContext> queryExp() {
			return getRuleContexts(QueryExpContext.class);
		}
		public QueryExpContext queryExp(int i) {
			return getRuleContext(QueryExpContext.class,i);
		}
		public OrderExpContext orderExp() {
			return getRuleContext(OrderExpContext.class,0);
		}
		public PageExpContext pageExp() {
			return getRuleContext(PageExpContext.class,0);
		}
		public QueryContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_query; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof AQLListener ) ((AQLListener)listener).enterQuery(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof AQLListener ) ((AQLListener)listener).exitQuery(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof AQLVisitor ) return ((AQLVisitor<? extends T>)visitor).visitQuery(this);
			else return visitor.visitChildren(this);
		}
	}

	public final QueryContext query() throws RecognitionException {
		QueryContext _localctx = new QueryContext(_ctx, getState());
		enterRule(_localctx, 0, RULE_query);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(33); 
			_errHandler.sync(this);
			_la = _input.LA(1);
			do {
				{
				{
				setState(32);
				queryExp(0);
				}
				}
				setState(35); 
				_errHandler.sync(this);
				_la = _input.LA(1);
			} while ( (((_la) & ~0x3f) == 0 && ((1L << _la) & ((1L << LAYOUT_COORDINATE_GITLFS_PATH) | (1L << LAYOUT_COORDINATE_MAVEN_EXTENSION) | (1L << LAYOUT_COORDINATE_MAVEN_GROUPID) | (1L << LAYOUT_COORDINATE_MAVEN_CLASSIFIER) | (1L << LAYOUT_COORDINATE_MAVEN_ARTIFACTID) | (1L << LAYOUT_COORDINATE_RAW_PATH) | (1L << LAYOUT_COORDINATE_NPM_EXTENSION) | (1L << LAYOUT_COORDINATE_NPM_SCOPE) | (1L << LAYOUT_COORDINATE_NPM_NAME) | (1L << LAYOUT_COORDINATE_NPM_DISTRIBUTION) | (1L << LAYOUT_COORDINATE_NUGET_ID) | (1L << LAYOUT_COORDINATE_NUGET_TYPE) | (1L << STORAGE) | (1L << REPOSITORY) | (1L << LAYOUT) | (1L << VERSION) | (1L << TAG) | (1L << FROM) | (1L << TO) | (1L << AGE) | (1L << PLUS) | (1L << NEGATION) | (1L << ROUND_BRACKET_LEFT))) != 0) );
			setState(38);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==ASC || _la==DESC) {
				{
				setState(37);
				orderExp();
				}
			}

			setState(41);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==PAGE_SKIP) {
				{
				setState(40);
				pageExp();
				}
			}

			setState(43);
			match(EOF);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class PageExpContext extends ParserRuleContext {
		public TerminalNode PAGE_SKIP() { return getToken(AQLParser.PAGE_SKIP, 0); }
		public TerminalNode COLON() { return getToken(AQLParser.COLON, 0); }
		public TerminalNode NUMBER() { return getToken(AQLParser.NUMBER, 0); }
		public PageExpContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_pageExp; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof AQLListener ) ((AQLListener)listener).enterPageExp(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof AQLListener ) ((AQLListener)listener).exitPageExp(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof AQLVisitor ) return ((AQLVisitor<? extends T>)visitor).visitPageExp(this);
			else return visitor.visitChildren(this);
		}
	}

	public final PageExpContext pageExp() throws RecognitionException {
		PageExpContext _localctx = new PageExpContext(_ctx, getState());
		enterRule(_localctx, 2, RULE_pageExp);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(45);
			match(PAGE_SKIP);
			setState(46);
			match(COLON);
			setState(47);
			match(NUMBER);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class OrderExpContext extends ParserRuleContext {
		public OrderDirectionContext orderDirection() {
			return getRuleContext(OrderDirectionContext.class,0);
		}
		public TerminalNode COLON() { return getToken(AQLParser.COLON, 0); }
		public OrderValueContext orderValue() {
			return getRuleContext(OrderValueContext.class,0);
		}
		public OrderExpContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_orderExp; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof AQLListener ) ((AQLListener)listener).enterOrderExp(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof AQLListener ) ((AQLListener)listener).exitOrderExp(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof AQLVisitor ) return ((AQLVisitor<? extends T>)visitor).visitOrderExp(this);
			else return visitor.visitChildren(this);
		}
	}

	public final OrderExpContext orderExp() throws RecognitionException {
		OrderExpContext _localctx = new OrderExpContext(_ctx, getState());
		enterRule(_localctx, 4, RULE_orderExp);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(49);
			orderDirection();
			setState(50);
			match(COLON);
			setState(51);
			orderValue();
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class OrderValueContext extends ParserRuleContext {
		public TerminalNode STORAGE() { return getToken(AQLParser.STORAGE, 0); }
		public TerminalNode REPOSITORY() { return getToken(AQLParser.REPOSITORY, 0); }
		public TerminalNode LAYOUT() { return getToken(AQLParser.LAYOUT, 0); }
		public TerminalNode VERSION() { return getToken(AQLParser.VERSION, 0); }
		public TerminalNode TAG() { return getToken(AQLParser.TAG, 0); }
		public TerminalNode AGE() { return getToken(AQLParser.AGE, 0); }
		public LayoutCoordinateKeywordContext layoutCoordinateKeyword() {
			return getRuleContext(LayoutCoordinateKeywordContext.class,0);
		}
		public OrderValueContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_orderValue; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof AQLListener ) ((AQLListener)listener).enterOrderValue(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof AQLListener ) ((AQLListener)listener).exitOrderValue(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof AQLVisitor ) return ((AQLVisitor<? extends T>)visitor).visitOrderValue(this);
			else return visitor.visitChildren(this);
		}
	}

	public final OrderValueContext orderValue() throws RecognitionException {
		OrderValueContext _localctx = new OrderValueContext(_ctx, getState());
		enterRule(_localctx, 6, RULE_orderValue);
		try {
			setState(60);
			_errHandler.sync(this);
			switch (_input.LA(1)) {
			case STORAGE:
				enterOuterAlt(_localctx, 1);
				{
				setState(53);
				match(STORAGE);
				}
				break;
			case REPOSITORY:
				enterOuterAlt(_localctx, 2);
				{
				setState(54);
				match(REPOSITORY);
				}
				break;
			case LAYOUT:
				enterOuterAlt(_localctx, 3);
				{
				setState(55);
				match(LAYOUT);
				}
				break;
			case VERSION:
				enterOuterAlt(_localctx, 4);
				{
				setState(56);
				match(VERSION);
				}
				break;
			case TAG:
				enterOuterAlt(_localctx, 5);
				{
				setState(57);
				match(TAG);
				}
				break;
			case AGE:
				enterOuterAlt(_localctx, 6);
				{
				setState(58);
				match(AGE);
				}
				break;
			case LAYOUT_COORDINATE_GITLFS_PATH:
			case LAYOUT_COORDINATE_MAVEN_EXTENSION:
			case LAYOUT_COORDINATE_MAVEN_GROUPID:
			case LAYOUT_COORDINATE_MAVEN_CLASSIFIER:
			case LAYOUT_COORDINATE_MAVEN_ARTIFACTID:
			case LAYOUT_COORDINATE_RAW_PATH:
			case LAYOUT_COORDINATE_NPM_EXTENSION:
			case LAYOUT_COORDINATE_NPM_SCOPE:
			case LAYOUT_COORDINATE_NPM_NAME:
			case LAYOUT_COORDINATE_NPM_DISTRIBUTION:
			case LAYOUT_COORDINATE_NUGET_ID:
			case LAYOUT_COORDINATE_NUGET_TYPE:
				enterOuterAlt(_localctx, 7);
				{
				setState(59);
				layoutCoordinateKeyword();
				}
				break;
			default:
				throw new NoViableAltException(this);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class OrderDirectionContext extends ParserRuleContext {
		public TerminalNode ASC() { return getToken(AQLParser.ASC, 0); }
		public TerminalNode DESC() { return getToken(AQLParser.DESC, 0); }
		public OrderDirectionContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_orderDirection; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof AQLListener ) ((AQLListener)listener).enterOrderDirection(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof AQLListener ) ((AQLListener)listener).exitOrderDirection(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof AQLVisitor ) return ((AQLVisitor<? extends T>)visitor).visitOrderDirection(this);
			else return visitor.visitChildren(this);
		}
	}

	public final OrderDirectionContext orderDirection() throws RecognitionException {
		OrderDirectionContext _localctx = new OrderDirectionContext(_ctx, getState());
		enterRule(_localctx, 8, RULE_orderDirection);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(62);
			_la = _input.LA(1);
			if ( !(_la==ASC || _la==DESC) ) {
			_errHandler.recoverInline(this);
			}
			else {
				if ( _input.LA(1)==Token.EOF ) matchedEOF = true;
				_errHandler.reportMatch(this);
				consume();
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class QueryExpContext extends ParserRuleContext {
		public QueryExpContext vQueryExpLeft;
		public QueryExpContext vNestedQueryExp;
		public QueryExpContext vQueryExpRight;
		public TerminalNode ROUND_BRACKET_LEFT() { return getToken(AQLParser.ROUND_BRACKET_LEFT, 0); }
		public TerminalNode ROUND_BRACKET_RIGHT() { return getToken(AQLParser.ROUND_BRACKET_RIGHT, 0); }
		public List<QueryExpContext> queryExp() {
			return getRuleContexts(QueryExpContext.class);
		}
		public QueryExpContext queryExp(int i) {
			return getRuleContext(QueryExpContext.class,i);
		}
		public TokenPrefixContext tokenPrefix() {
			return getRuleContext(TokenPrefixContext.class,0);
		}
		public TokenExpContext tokenExp() {
			return getRuleContext(TokenExpContext.class,0);
		}
		public LogicalOpContext logicalOp() {
			return getRuleContext(LogicalOpContext.class,0);
		}
		public QueryExpContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_queryExp; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof AQLListener ) ((AQLListener)listener).enterQueryExp(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof AQLListener ) ((AQLListener)listener).exitQueryExp(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof AQLVisitor ) return ((AQLVisitor<? extends T>)visitor).visitQueryExp(this);
			else return visitor.visitChildren(this);
		}
	}

	public final QueryExpContext queryExp() throws RecognitionException {
		return queryExp(0);
	}

	private QueryExpContext queryExp(int _p) throws RecognitionException {
		ParserRuleContext _parentctx = _ctx;
		int _parentState = getState();
		QueryExpContext _localctx = new QueryExpContext(_ctx, _parentState);
		QueryExpContext _prevctx = _localctx;
		int _startState = 10;
		enterRecursionRule(_localctx, 10, RULE_queryExp, _p);
		int _la;
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(76);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,6,_ctx) ) {
			case 1:
				{
				setState(66);
				_errHandler.sync(this);
				_la = _input.LA(1);
				if (_la==PLUS || _la==NEGATION) {
					{
					setState(65);
					tokenPrefix();
					}
				}

				setState(68);
				match(ROUND_BRACKET_LEFT);
				setState(69);
				((QueryExpContext)_localctx).vNestedQueryExp = queryExp(0);
				setState(70);
				match(ROUND_BRACKET_RIGHT);
				}
				break;
			case 2:
				{
				setState(73);
				_errHandler.sync(this);
				_la = _input.LA(1);
				if (_la==PLUS || _la==NEGATION) {
					{
					setState(72);
					tokenPrefix();
					}
				}

				setState(75);
				tokenExp();
				}
				break;
			}
			_ctx.stop = _input.LT(-1);
			setState(85);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,8,_ctx);
			while ( _alt!=2 && _alt!= ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					if ( _parseListeners!=null ) triggerExitRuleEvent();
					_prevctx = _localctx;
					{
					{
					_localctx = new QueryExpContext(_parentctx, _parentState);
					_localctx.vQueryExpLeft = _prevctx;
					_localctx.vQueryExpLeft = _prevctx;
					pushNewRecursionContext(_localctx, _startState, RULE_queryExp);
					setState(78);
					if (!(precpred(_ctx, 2))) throw new FailedPredicateException(this, "precpred(_ctx, 2)");
					setState(80);
					_errHandler.sync(this);
					_la = _input.LA(1);
					if ((((_la) & ~0x3f) == 0 && ((1L << _la) & ((1L << AMP) | (1L << DOUBLE_AMP) | (1L << AND) | (1L << PIPE) | (1L << DOUBLE_PIPE) | (1L << OR))) != 0)) {
						{
						setState(79);
						logicalOp();
						}
					}

					setState(82);
					((QueryExpContext)_localctx).vQueryExpRight = queryExp(3);
					}
					} 
				}
				setState(87);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,8,_ctx);
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			unrollRecursionContexts(_parentctx);
		}
		return _localctx;
	}

	public static class TokenExpContext extends ParserRuleContext {
		public TerminalNode LAYOUT() { return getToken(AQLParser.LAYOUT, 0); }
		public TerminalNode COLON() { return getToken(AQLParser.COLON, 0); }
		public LayoutValueContext layoutValue() {
			return getRuleContext(LayoutValueContext.class,0);
		}
		public TokenKeyContext tokenKey() {
			return getRuleContext(TokenKeyContext.class,0);
		}
		public TokenValueContext tokenValue() {
			return getRuleContext(TokenValueContext.class,0);
		}
		public TokenExpContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_tokenExp; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof AQLListener ) ((AQLListener)listener).enterTokenExp(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof AQLListener ) ((AQLListener)listener).exitTokenExp(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof AQLVisitor ) return ((AQLVisitor<? extends T>)visitor).visitTokenExp(this);
			else return visitor.visitChildren(this);
		}
	}

	public final TokenExpContext tokenExp() throws RecognitionException {
		TokenExpContext _localctx = new TokenExpContext(_ctx, getState());
		enterRule(_localctx, 12, RULE_tokenExp);
		try {
			setState(95);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,9,_ctx) ) {
			case 1:
				enterOuterAlt(_localctx, 1);
				{
				setState(88);
				match(LAYOUT);
				setState(89);
				match(COLON);
				setState(90);
				layoutValue();
				}
				break;
			case 2:
				enterOuterAlt(_localctx, 2);
				{
				setState(91);
				tokenKey();
				setState(92);
				match(COLON);
				setState(93);
				tokenValue();
				}
				break;
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class TokenPrefixContext extends ParserRuleContext {
		public TerminalNode PLUS() { return getToken(AQLParser.PLUS, 0); }
		public TerminalNode NEGATION() { return getToken(AQLParser.NEGATION, 0); }
		public TokenPrefixContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_tokenPrefix; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof AQLListener ) ((AQLListener)listener).enterTokenPrefix(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof AQLListener ) ((AQLListener)listener).exitTokenPrefix(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof AQLVisitor ) return ((AQLVisitor<? extends T>)visitor).visitTokenPrefix(this);
			else return visitor.visitChildren(this);
		}
	}

	public final TokenPrefixContext tokenPrefix() throws RecognitionException {
		TokenPrefixContext _localctx = new TokenPrefixContext(_ctx, getState());
		enterRule(_localctx, 14, RULE_tokenPrefix);
		try {
			setState(101);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,10,_ctx) ) {
			case 1:
				enterOuterAlt(_localctx, 1);
				{
				setState(97);
				match(PLUS);
				}
				break;
			case 2:
				enterOuterAlt(_localctx, 2);
				{
				setState(98);
				match(NEGATION);
				}
				break;
			case 3:
				enterOuterAlt(_localctx, 3);
				{
				setState(99);
				match(PLUS);
				setState(100);
				match(NEGATION);
				}
				break;
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class TokenKeyContext extends ParserRuleContext {
		public TokenKeywordContext tokenKeyword() {
			return getRuleContext(TokenKeywordContext.class,0);
		}
		public LayoutCoordinateKeywordContext layoutCoordinateKeyword() {
			return getRuleContext(LayoutCoordinateKeywordContext.class,0);
		}
		public TokenKeyContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_tokenKey; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof AQLListener ) ((AQLListener)listener).enterTokenKey(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof AQLListener ) ((AQLListener)listener).exitTokenKey(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof AQLVisitor ) return ((AQLVisitor<? extends T>)visitor).visitTokenKey(this);
			else return visitor.visitChildren(this);
		}
	}

	public final TokenKeyContext tokenKey() throws RecognitionException {
		TokenKeyContext _localctx = new TokenKeyContext(_ctx, getState());
		enterRule(_localctx, 16, RULE_tokenKey);
		try {
			setState(105);
			_errHandler.sync(this);
			switch (_input.LA(1)) {
			case STORAGE:
			case REPOSITORY:
			case LAYOUT:
			case VERSION:
			case TAG:
			case FROM:
			case TO:
			case AGE:
				enterOuterAlt(_localctx, 1);
				{
				setState(103);
				tokenKeyword();
				}
				break;
			case LAYOUT_COORDINATE_GITLFS_PATH:
			case LAYOUT_COORDINATE_MAVEN_EXTENSION:
			case LAYOUT_COORDINATE_MAVEN_GROUPID:
			case LAYOUT_COORDINATE_MAVEN_CLASSIFIER:
			case LAYOUT_COORDINATE_MAVEN_ARTIFACTID:
			case LAYOUT_COORDINATE_RAW_PATH:
			case LAYOUT_COORDINATE_NPM_EXTENSION:
			case LAYOUT_COORDINATE_NPM_SCOPE:
			case LAYOUT_COORDINATE_NPM_NAME:
			case LAYOUT_COORDINATE_NPM_DISTRIBUTION:
			case LAYOUT_COORDINATE_NUGET_ID:
			case LAYOUT_COORDINATE_NUGET_TYPE:
				enterOuterAlt(_localctx, 2);
				{
				setState(104);
				layoutCoordinateKeyword();
				}
				break;
			default:
				throw new NoViableAltException(this);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class LayoutValueContext extends ParserRuleContext {
		public TerminalNode LAYOUT_GITLFS() { return getToken(AQLParser.LAYOUT_GITLFS, 0); }
		public TerminalNode LAYOUT_MAVEN() { return getToken(AQLParser.LAYOUT_MAVEN, 0); }
		public TerminalNode LAYOUT_RAW() { return getToken(AQLParser.LAYOUT_RAW, 0); }
		public TerminalNode LAYOUT_NPM() { return getToken(AQLParser.LAYOUT_NPM, 0); }
		public TerminalNode LAYOUT_NUGET() { return getToken(AQLParser.LAYOUT_NUGET, 0); }
		public LayoutValueContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_layoutValue; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof AQLListener ) ((AQLListener)listener).enterLayoutValue(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof AQLListener ) ((AQLListener)listener).exitLayoutValue(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof AQLVisitor ) return ((AQLVisitor<? extends T>)visitor).visitLayoutValue(this);
			else return visitor.visitChildren(this);
		}
	}

	public final LayoutValueContext layoutValue() throws RecognitionException {
		LayoutValueContext _localctx = new LayoutValueContext(_ctx, getState());
		enterRule(_localctx, 18, RULE_layoutValue);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(107);
			_la = _input.LA(1);
			if ( !((((_la) & ~0x3f) == 0 && ((1L << _la) & ((1L << LAYOUT_GITLFS) | (1L << LAYOUT_MAVEN) | (1L << LAYOUT_RAW) | (1L << LAYOUT_NPM) | (1L << LAYOUT_NUGET))) != 0)) ) {
			_errHandler.recoverInline(this);
			}
			else {
				if ( _input.LA(1)==Token.EOF ) matchedEOF = true;
				_errHandler.reportMatch(this);
				consume();
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class TokenValueContext extends ParserRuleContext {
		public TerminalNode VALUE() { return getToken(AQLParser.VALUE, 0); }
		public TerminalNode STRING() { return getToken(AQLParser.STRING, 0); }
		public TokenValueContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_tokenValue; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof AQLListener ) ((AQLListener)listener).enterTokenValue(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof AQLListener ) ((AQLListener)listener).exitTokenValue(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof AQLVisitor ) return ((AQLVisitor<? extends T>)visitor).visitTokenValue(this);
			else return visitor.visitChildren(this);
		}
	}

	public final TokenValueContext tokenValue() throws RecognitionException {
		TokenValueContext _localctx = new TokenValueContext(_ctx, getState());
		enterRule(_localctx, 20, RULE_tokenValue);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(109);
			_la = _input.LA(1);
			if ( !(_la==VALUE || _la==STRING) ) {
			_errHandler.recoverInline(this);
			}
			else {
				if ( _input.LA(1)==Token.EOF ) matchedEOF = true;
				_errHandler.reportMatch(this);
				consume();
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class TokenKeywordContext extends ParserRuleContext {
		public TerminalNode STORAGE() { return getToken(AQLParser.STORAGE, 0); }
		public TerminalNode REPOSITORY() { return getToken(AQLParser.REPOSITORY, 0); }
		public TerminalNode LAYOUT() { return getToken(AQLParser.LAYOUT, 0); }
		public TerminalNode VERSION() { return getToken(AQLParser.VERSION, 0); }
		public TerminalNode TAG() { return getToken(AQLParser.TAG, 0); }
		public TerminalNode FROM() { return getToken(AQLParser.FROM, 0); }
		public TerminalNode TO() { return getToken(AQLParser.TO, 0); }
		public TerminalNode AGE() { return getToken(AQLParser.AGE, 0); }
		public TokenKeywordContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_tokenKeyword; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof AQLListener ) ((AQLListener)listener).enterTokenKeyword(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof AQLListener ) ((AQLListener)listener).exitTokenKeyword(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof AQLVisitor ) return ((AQLVisitor<? extends T>)visitor).visitTokenKeyword(this);
			else return visitor.visitChildren(this);
		}
	}

	public final TokenKeywordContext tokenKeyword() throws RecognitionException {
		TokenKeywordContext _localctx = new TokenKeywordContext(_ctx, getState());
		enterRule(_localctx, 22, RULE_tokenKeyword);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(111);
			_la = _input.LA(1);
			if ( !((((_la) & ~0x3f) == 0 && ((1L << _la) & ((1L << STORAGE) | (1L << REPOSITORY) | (1L << LAYOUT) | (1L << VERSION) | (1L << TAG) | (1L << FROM) | (1L << TO) | (1L << AGE))) != 0)) ) {
			_errHandler.recoverInline(this);
			}
			else {
				if ( _input.LA(1)==Token.EOF ) matchedEOF = true;
				_errHandler.reportMatch(this);
				consume();
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class LayoutCoordinateKeywordContext extends ParserRuleContext {
		public TerminalNode LAYOUT_COORDINATE_GITLFS_PATH() { return getToken(AQLParser.LAYOUT_COORDINATE_GITLFS_PATH, 0); }
		public TerminalNode LAYOUT_COORDINATE_MAVEN_EXTENSION() { return getToken(AQLParser.LAYOUT_COORDINATE_MAVEN_EXTENSION, 0); }
		public TerminalNode LAYOUT_COORDINATE_MAVEN_GROUPID() { return getToken(AQLParser.LAYOUT_COORDINATE_MAVEN_GROUPID, 0); }
		public TerminalNode LAYOUT_COORDINATE_MAVEN_CLASSIFIER() { return getToken(AQLParser.LAYOUT_COORDINATE_MAVEN_CLASSIFIER, 0); }
		public TerminalNode LAYOUT_COORDINATE_MAVEN_ARTIFACTID() { return getToken(AQLParser.LAYOUT_COORDINATE_MAVEN_ARTIFACTID, 0); }
		public TerminalNode LAYOUT_COORDINATE_RAW_PATH() { return getToken(AQLParser.LAYOUT_COORDINATE_RAW_PATH, 0); }
		public TerminalNode LAYOUT_COORDINATE_NPM_EXTENSION() { return getToken(AQLParser.LAYOUT_COORDINATE_NPM_EXTENSION, 0); }
		public TerminalNode LAYOUT_COORDINATE_NPM_SCOPE() { return getToken(AQLParser.LAYOUT_COORDINATE_NPM_SCOPE, 0); }
		public TerminalNode LAYOUT_COORDINATE_NPM_NAME() { return getToken(AQLParser.LAYOUT_COORDINATE_NPM_NAME, 0); }
		public TerminalNode LAYOUT_COORDINATE_NPM_DISTRIBUTION() { return getToken(AQLParser.LAYOUT_COORDINATE_NPM_DISTRIBUTION, 0); }
		public TerminalNode LAYOUT_COORDINATE_NUGET_ID() { return getToken(AQLParser.LAYOUT_COORDINATE_NUGET_ID, 0); }
		public TerminalNode LAYOUT_COORDINATE_NUGET_TYPE() { return getToken(AQLParser.LAYOUT_COORDINATE_NUGET_TYPE, 0); }
		public LayoutCoordinateKeywordContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_layoutCoordinateKeyword; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof AQLListener ) ((AQLListener)listener).enterLayoutCoordinateKeyword(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof AQLListener ) ((AQLListener)listener).exitLayoutCoordinateKeyword(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof AQLVisitor ) return ((AQLVisitor<? extends T>)visitor).visitLayoutCoordinateKeyword(this);
			else return visitor.visitChildren(this);
		}
	}

	public final LayoutCoordinateKeywordContext layoutCoordinateKeyword() throws RecognitionException {
		LayoutCoordinateKeywordContext _localctx = new LayoutCoordinateKeywordContext(_ctx, getState());
		enterRule(_localctx, 24, RULE_layoutCoordinateKeyword);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(113);
			_la = _input.LA(1);
			if ( !((((_la) & ~0x3f) == 0 && ((1L << _la) & ((1L << LAYOUT_COORDINATE_GITLFS_PATH) | (1L << LAYOUT_COORDINATE_MAVEN_EXTENSION) | (1L << LAYOUT_COORDINATE_MAVEN_GROUPID) | (1L << LAYOUT_COORDINATE_MAVEN_CLASSIFIER) | (1L << LAYOUT_COORDINATE_MAVEN_ARTIFACTID) | (1L << LAYOUT_COORDINATE_RAW_PATH) | (1L << LAYOUT_COORDINATE_NPM_EXTENSION) | (1L << LAYOUT_COORDINATE_NPM_SCOPE) | (1L << LAYOUT_COORDINATE_NPM_NAME) | (1L << LAYOUT_COORDINATE_NPM_DISTRIBUTION) | (1L << LAYOUT_COORDINATE_NUGET_ID) | (1L << LAYOUT_COORDINATE_NUGET_TYPE))) != 0)) ) {
			_errHandler.recoverInline(this);
			}
			else {
				if ( _input.LA(1)==Token.EOF ) matchedEOF = true;
				_errHandler.reportMatch(this);
				consume();
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class LogicalOpContext extends ParserRuleContext {
		public AndContext and() {
			return getRuleContext(AndContext.class,0);
		}
		public OrContext or() {
			return getRuleContext(OrContext.class,0);
		}
		public LogicalOpContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_logicalOp; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof AQLListener ) ((AQLListener)listener).enterLogicalOp(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof AQLListener ) ((AQLListener)listener).exitLogicalOp(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof AQLVisitor ) return ((AQLVisitor<? extends T>)visitor).visitLogicalOp(this);
			else return visitor.visitChildren(this);
		}
	}

	public final LogicalOpContext logicalOp() throws RecognitionException {
		LogicalOpContext _localctx = new LogicalOpContext(_ctx, getState());
		enterRule(_localctx, 26, RULE_logicalOp);
		try {
			setState(117);
			_errHandler.sync(this);
			switch (_input.LA(1)) {
			case AMP:
			case DOUBLE_AMP:
			case AND:
				enterOuterAlt(_localctx, 1);
				{
				setState(115);
				and();
				}
				break;
			case PIPE:
			case DOUBLE_PIPE:
			case OR:
				enterOuterAlt(_localctx, 2);
				{
				setState(116);
				or();
				}
				break;
			default:
				throw new NoViableAltException(this);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class AndContext extends ParserRuleContext {
		public TerminalNode AND() { return getToken(AQLParser.AND, 0); }
		public TerminalNode AMP() { return getToken(AQLParser.AMP, 0); }
		public TerminalNode DOUBLE_AMP() { return getToken(AQLParser.DOUBLE_AMP, 0); }
		public AndContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_and; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof AQLListener ) ((AQLListener)listener).enterAnd(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof AQLListener ) ((AQLListener)listener).exitAnd(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof AQLVisitor ) return ((AQLVisitor<? extends T>)visitor).visitAnd(this);
			else return visitor.visitChildren(this);
		}
	}

	public final AndContext and() throws RecognitionException {
		AndContext _localctx = new AndContext(_ctx, getState());
		enterRule(_localctx, 28, RULE_and);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(119);
			_la = _input.LA(1);
			if ( !((((_la) & ~0x3f) == 0 && ((1L << _la) & ((1L << AMP) | (1L << DOUBLE_AMP) | (1L << AND))) != 0)) ) {
			_errHandler.recoverInline(this);
			}
			else {
				if ( _input.LA(1)==Token.EOF ) matchedEOF = true;
				_errHandler.reportMatch(this);
				consume();
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class OrContext extends ParserRuleContext {
		public TerminalNode OR() { return getToken(AQLParser.OR, 0); }
		public TerminalNode PIPE() { return getToken(AQLParser.PIPE, 0); }
		public TerminalNode DOUBLE_PIPE() { return getToken(AQLParser.DOUBLE_PIPE, 0); }
		public OrContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_or; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof AQLListener ) ((AQLListener)listener).enterOr(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof AQLListener ) ((AQLListener)listener).exitOr(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof AQLVisitor ) return ((AQLVisitor<? extends T>)visitor).visitOr(this);
			else return visitor.visitChildren(this);
		}
	}

	public final OrContext or() throws RecognitionException {
		OrContext _localctx = new OrContext(_ctx, getState());
		enterRule(_localctx, 30, RULE_or);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(121);
			_la = _input.LA(1);
			if ( !((((_la) & ~0x3f) == 0 && ((1L << _la) & ((1L << PIPE) | (1L << DOUBLE_PIPE) | (1L << OR))) != 0)) ) {
			_errHandler.recoverInline(this);
			}
			else {
				if ( _input.LA(1)==Token.EOF ) matchedEOF = true;
				_errHandler.reportMatch(this);
				consume();
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public boolean sempred(RuleContext _localctx, int ruleIndex, int predIndex) {
		switch (ruleIndex) {
		case 5:
			return queryExp_sempred((QueryExpContext)_localctx, predIndex);
		}
		return true;
	}
	private boolean queryExp_sempred(QueryExpContext _localctx, int predIndex) {
		switch (predIndex) {
		case 0:
			return precpred(_ctx, 2);
		}
		return true;
	}

	public static final String _serializedATN =
		"\3\u608b\ua72a\u8133\ub9ed\u417c\u3be7\u7786\u5964\3-~\4\2\t\2\4\3\t\3"+
		"\4\4\t\4\4\5\t\5\4\6\t\6\4\7\t\7\4\b\t\b\4\t\t\t\4\n\t\n\4\13\t\13\4\f"+
		"\t\f\4\r\t\r\4\16\t\16\4\17\t\17\4\20\t\20\4\21\t\21\3\2\6\2$\n\2\r\2"+
		"\16\2%\3\2\5\2)\n\2\3\2\5\2,\n\2\3\2\3\2\3\3\3\3\3\3\3\3\3\4\3\4\3\4\3"+
		"\4\3\5\3\5\3\5\3\5\3\5\3\5\3\5\5\5?\n\5\3\6\3\6\3\7\3\7\5\7E\n\7\3\7\3"+
		"\7\3\7\3\7\3\7\5\7L\n\7\3\7\5\7O\n\7\3\7\3\7\5\7S\n\7\3\7\7\7V\n\7\f\7"+
		"\16\7Y\13\7\3\b\3\b\3\b\3\b\3\b\3\b\3\b\5\bb\n\b\3\t\3\t\3\t\3\t\5\th"+
		"\n\t\3\n\3\n\5\nl\n\n\3\13\3\13\3\f\3\f\3\r\3\r\3\16\3\16\3\17\3\17\5"+
		"\17x\n\17\3\20\3\20\3\21\3\21\3\21\2\3\f\22\2\4\6\b\n\f\16\20\22\24\26"+
		"\30\32\34\36 \2\t\3\2\34\35\3\2\3\7\3\2+,\3\2\24\33\3\2\b\23\3\2\37!\3"+
		"\2\"$\2\u0080\2#\3\2\2\2\4/\3\2\2\2\6\63\3\2\2\2\b>\3\2\2\2\n@\3\2\2\2"+
		"\fN\3\2\2\2\16a\3\2\2\2\20g\3\2\2\2\22k\3\2\2\2\24m\3\2\2\2\26o\3\2\2"+
		"\2\30q\3\2\2\2\32s\3\2\2\2\34w\3\2\2\2\36y\3\2\2\2 {\3\2\2\2\"$\5\f\7"+
		"\2#\"\3\2\2\2$%\3\2\2\2%#\3\2\2\2%&\3\2\2\2&(\3\2\2\2\')\5\6\4\2(\'\3"+
		"\2\2\2()\3\2\2\2)+\3\2\2\2*,\5\4\3\2+*\3\2\2\2+,\3\2\2\2,-\3\2\2\2-.\7"+
		"\2\2\3.\3\3\2\2\2/\60\7\36\2\2\60\61\7\'\2\2\61\62\7*\2\2\62\5\3\2\2\2"+
		"\63\64\5\n\6\2\64\65\7\'\2\2\65\66\5\b\5\2\66\7\3\2\2\2\67?\7\24\2\28"+
		"?\7\25\2\29?\7\26\2\2:?\7\27\2\2;?\7\30\2\2<?\7\33\2\2=?\5\32\16\2>\67"+
		"\3\2\2\2>8\3\2\2\2>9\3\2\2\2>:\3\2\2\2>;\3\2\2\2><\3\2\2\2>=\3\2\2\2?"+
		"\t\3\2\2\2@A\t\2\2\2A\13\3\2\2\2BD\b\7\1\2CE\5\20\t\2DC\3\2\2\2DE\3\2"+
		"\2\2EF\3\2\2\2FG\7(\2\2GH\5\f\7\2HI\7)\2\2IO\3\2\2\2JL\5\20\t\2KJ\3\2"+
		"\2\2KL\3\2\2\2LM\3\2\2\2MO\5\16\b\2NB\3\2\2\2NK\3\2\2\2OW\3\2\2\2PR\f"+
		"\4\2\2QS\5\34\17\2RQ\3\2\2\2RS\3\2\2\2ST\3\2\2\2TV\5\f\7\5UP\3\2\2\2V"+
		"Y\3\2\2\2WU\3\2\2\2WX\3\2\2\2X\r\3\2\2\2YW\3\2\2\2Z[\7\26\2\2[\\\7\'\2"+
		"\2\\b\5\24\13\2]^\5\22\n\2^_\7\'\2\2_`\5\26\f\2`b\3\2\2\2aZ\3\2\2\2a]"+
		"\3\2\2\2b\17\3\2\2\2ch\7%\2\2dh\7&\2\2ef\7%\2\2fh\7&\2\2gc\3\2\2\2gd\3"+
		"\2\2\2ge\3\2\2\2h\21\3\2\2\2il\5\30\r\2jl\5\32\16\2ki\3\2\2\2kj\3\2\2"+
		"\2l\23\3\2\2\2mn\t\3\2\2n\25\3\2\2\2op\t\4\2\2p\27\3\2\2\2qr\t\5\2\2r"+
		"\31\3\2\2\2st\t\6\2\2t\33\3\2\2\2ux\5\36\20\2vx\5 \21\2wu\3\2\2\2wv\3"+
		"\2\2\2x\35\3\2\2\2yz\t\7\2\2z\37\3\2\2\2{|\t\b\2\2|!\3\2\2\2\17%(+>DK"+
		"NRWagkw";
	public static final ATN _ATN =
		new ATNDeserializer().deserialize(_serializedATN.toCharArray());
	static {
		_decisionToDFA = new DFA[_ATN.getNumberOfDecisions()];
		for (int i = 0; i < _ATN.getNumberOfDecisions(); i++) {
			_decisionToDFA[i] = new DFA(_ATN.getDecisionState(i), i);
		}
	}
}