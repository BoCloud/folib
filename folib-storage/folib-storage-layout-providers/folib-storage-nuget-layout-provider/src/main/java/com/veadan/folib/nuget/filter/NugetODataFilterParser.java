// Generated from com/veadan/folib/nuget/filter/NugetODataFilter.g4 by ANTLR 4.7.1
package com.veadan.folib.nuget.filter;

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
public class NugetODataFilterParser extends Parser {
	static { RuntimeMetaData.checkVersion("4.7.1", RuntimeMetaData.VERSION); }

	protected static final DFA[] _decisionToDFA;
	protected static final PredictionContextCache _sharedContextCache =
		new PredictionContextCache();
	public static final int
		T__0=1, T__1=2, T__2=3, TO_LOWER=4, TAG=5, ATTRIBUTE=6, EQ=7, GE=8, AND=9, 
		OR=10, NOT=11, VALUE=12, WHITESPACE=13;
	public static final int
		RULE_filter = 0, RULE_filterExp = 1, RULE_tokenExp = 2, RULE_tokenExpRight = 3, 
		RULE_tokenExpLeft = 4, RULE_tokenExpFunction = 5, RULE_fuctionExp = 6, 
		RULE_filterOp = 7, RULE_logicalOp = 8;
	public static final String[] ruleNames = {
		"filter", "filterExp", "tokenExp", "tokenExpRight", "tokenExpLeft", "tokenExpFunction", 
		"fuctionExp", "filterOp", "logicalOp"
	};

	private static final String[] _LITERAL_NAMES = {
		null, "'('", "')'", "'''", "'tolower'", "'IsLatestVersion'", null, "'eq'", 
		"'ge'", "'and'", "'or'", "'not'", null, "' '"
	};
	private static final String[] _SYMBOLIC_NAMES = {
		null, null, null, null, "TO_LOWER", "TAG", "ATTRIBUTE", "EQ", "GE", "AND", 
		"OR", "NOT", "VALUE", "WHITESPACE"
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
	public String getGrammarFileName() { return "NugetODataFilter.g4"; }

	@Override
	public String[] getRuleNames() { return ruleNames; }

	@Override
	public String getSerializedATN() { return _serializedATN; }

	@Override
	public ATN getATN() { return _ATN; }

	public NugetODataFilterParser(TokenStream input) {
		super(input);
		_interp = new ParserATNSimulator(this,_ATN,_decisionToDFA,_sharedContextCache);
	}
	public static class FilterContext extends ParserRuleContext {
		public FilterExpContext filterExp() {
			return getRuleContext(FilterExpContext.class,0);
		}
		public FilterContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_filter; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof NugetODataFilterListener ) ((NugetODataFilterListener)listener).enterFilter(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof NugetODataFilterListener ) ((NugetODataFilterListener)listener).exitFilter(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof NugetODataFilterVisitor ) return ((NugetODataFilterVisitor<? extends T>)visitor).visitFilter(this);
			else return visitor.visitChildren(this);
		}
	}

	public final FilterContext filter() throws RecognitionException {
		FilterContext _localctx = new FilterContext(_ctx, getState());
		enterRule(_localctx, 0, RULE_filter);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(18);
			filterExp(0);
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

	public static class FilterExpContext extends ParserRuleContext {
		public FilterExpContext vFilterExpLeft;
		public FilterExpContext vNestedFilterExp;
		public LogicalOpContext vLogicalOp;
		public FilterExpContext vFilterExpRight;
		public List<FilterExpContext> filterExp() {
			return getRuleContexts(FilterExpContext.class);
		}
		public FilterExpContext filterExp(int i) {
			return getRuleContext(FilterExpContext.class,i);
		}
		public TokenExpContext tokenExp() {
			return getRuleContext(TokenExpContext.class,0);
		}
		public LogicalOpContext logicalOp() {
			return getRuleContext(LogicalOpContext.class,0);
		}
		public FilterExpContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_filterExp; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof NugetODataFilterListener ) ((NugetODataFilterListener)listener).enterFilterExp(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof NugetODataFilterListener ) ((NugetODataFilterListener)listener).exitFilterExp(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof NugetODataFilterVisitor ) return ((NugetODataFilterVisitor<? extends T>)visitor).visitFilterExp(this);
			else return visitor.visitChildren(this);
		}
	}

	public final FilterExpContext filterExp() throws RecognitionException {
		return filterExp(0);
	}

	private FilterExpContext filterExp(int _p) throws RecognitionException {
		ParserRuleContext _parentctx = _ctx;
		int _parentState = getState();
		FilterExpContext _localctx = new FilterExpContext(_ctx, _parentState);
		FilterExpContext _prevctx = _localctx;
		int _startState = 2;
		enterRecursionRule(_localctx, 2, RULE_filterExp, _p);
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(26);
			_errHandler.sync(this);
			switch (_input.LA(1)) {
			case T__0:
				{
				setState(21);
				match(T__0);
				setState(22);
				((FilterExpContext)_localctx).vNestedFilterExp = filterExp(0);
				setState(23);
				match(T__1);
				}
				break;
			case TO_LOWER:
			case TAG:
			case ATTRIBUTE:
				{
				setState(25);
				tokenExp();
				}
				break;
			default:
				throw new NoViableAltException(this);
			}
			_ctx.stop = _input.LT(-1);
			setState(34);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,1,_ctx);
			while ( _alt!=2 && _alt!= ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					if ( _parseListeners!=null ) triggerExitRuleEvent();
					_prevctx = _localctx;
					{
					{
					_localctx = new FilterExpContext(_parentctx, _parentState);
					_localctx.vFilterExpLeft = _prevctx;
					_localctx.vFilterExpLeft = _prevctx;
					pushNewRecursionContext(_localctx, _startState, RULE_filterExp);
					setState(28);
					if (!(precpred(_ctx, 2))) throw new FailedPredicateException(this, "precpred(_ctx, 2)");
					setState(29);
					((FilterExpContext)_localctx).vLogicalOp = logicalOp();
					setState(30);
					((FilterExpContext)_localctx).vFilterExpRight = filterExp(3);
					}
					} 
				}
				setState(36);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,1,_ctx);
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
		public TokenExpLeftContext vTokenExpLeft;
		public FilterOpContext vFilterOp;
		public TokenExpRightContext vTokenExpRight;
		public TokenExpLeftContext tokenExpLeft() {
			return getRuleContext(TokenExpLeftContext.class,0);
		}
		public FilterOpContext filterOp() {
			return getRuleContext(FilterOpContext.class,0);
		}
		public TokenExpRightContext tokenExpRight() {
			return getRuleContext(TokenExpRightContext.class,0);
		}
		public TerminalNode TAG() { return getToken(NugetODataFilterParser.TAG, 0); }
		public TokenExpContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_tokenExp; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof NugetODataFilterListener ) ((NugetODataFilterListener)listener).enterTokenExp(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof NugetODataFilterListener ) ((NugetODataFilterListener)listener).exitTokenExp(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof NugetODataFilterVisitor ) return ((NugetODataFilterVisitor<? extends T>)visitor).visitTokenExp(this);
			else return visitor.visitChildren(this);
		}
	}

	public final TokenExpContext tokenExp() throws RecognitionException {
		TokenExpContext _localctx = new TokenExpContext(_ctx, getState());
		enterRule(_localctx, 4, RULE_tokenExp);
		try {
			setState(42);
			_errHandler.sync(this);
			switch (_input.LA(1)) {
			case TO_LOWER:
			case ATTRIBUTE:
				enterOuterAlt(_localctx, 1);
				{
				setState(37);
				((TokenExpContext)_localctx).vTokenExpLeft = tokenExpLeft();
				setState(38);
				((TokenExpContext)_localctx).vFilterOp = filterOp();
				setState(39);
				((TokenExpContext)_localctx).vTokenExpRight = tokenExpRight();
				}
				break;
			case TAG:
				enterOuterAlt(_localctx, 2);
				{
				setState(41);
				match(TAG);
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

	public static class TokenExpRightContext extends ParserRuleContext {
		public TerminalNode VALUE() { return getToken(NugetODataFilterParser.VALUE, 0); }
		public TokenExpRightContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_tokenExpRight; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof NugetODataFilterListener ) ((NugetODataFilterListener)listener).enterTokenExpRight(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof NugetODataFilterListener ) ((NugetODataFilterListener)listener).exitTokenExpRight(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof NugetODataFilterVisitor ) return ((NugetODataFilterVisitor<? extends T>)visitor).visitTokenExpRight(this);
			else return visitor.visitChildren(this);
		}
	}

	public final TokenExpRightContext tokenExpRight() throws RecognitionException {
		TokenExpRightContext _localctx = new TokenExpRightContext(_ctx, getState());
		enterRule(_localctx, 6, RULE_tokenExpRight);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(44);
			match(T__2);
			setState(45);
			match(VALUE);
			setState(46);
			match(T__2);
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

	public static class TokenExpLeftContext extends ParserRuleContext {
		public TerminalNode ATTRIBUTE() { return getToken(NugetODataFilterParser.ATTRIBUTE, 0); }
		public TokenExpFunctionContext tokenExpFunction() {
			return getRuleContext(TokenExpFunctionContext.class,0);
		}
		public TokenExpLeftContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_tokenExpLeft; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof NugetODataFilterListener ) ((NugetODataFilterListener)listener).enterTokenExpLeft(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof NugetODataFilterListener ) ((NugetODataFilterListener)listener).exitTokenExpLeft(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof NugetODataFilterVisitor ) return ((NugetODataFilterVisitor<? extends T>)visitor).visitTokenExpLeft(this);
			else return visitor.visitChildren(this);
		}
	}

	public final TokenExpLeftContext tokenExpLeft() throws RecognitionException {
		TokenExpLeftContext _localctx = new TokenExpLeftContext(_ctx, getState());
		enterRule(_localctx, 8, RULE_tokenExpLeft);
		try {
			setState(50);
			_errHandler.sync(this);
			switch (_input.LA(1)) {
			case ATTRIBUTE:
				enterOuterAlt(_localctx, 1);
				{
				setState(48);
				match(ATTRIBUTE);
				}
				break;
			case TO_LOWER:
				enterOuterAlt(_localctx, 2);
				{
				setState(49);
				tokenExpFunction();
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

	public static class TokenExpFunctionContext extends ParserRuleContext {
		public FuctionExpContext fuctionExp() {
			return getRuleContext(FuctionExpContext.class,0);
		}
		public TerminalNode ATTRIBUTE() { return getToken(NugetODataFilterParser.ATTRIBUTE, 0); }
		public TokenExpFunctionContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_tokenExpFunction; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof NugetODataFilterListener ) ((NugetODataFilterListener)listener).enterTokenExpFunction(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof NugetODataFilterListener ) ((NugetODataFilterListener)listener).exitTokenExpFunction(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof NugetODataFilterVisitor ) return ((NugetODataFilterVisitor<? extends T>)visitor).visitTokenExpFunction(this);
			else return visitor.visitChildren(this);
		}
	}

	public final TokenExpFunctionContext tokenExpFunction() throws RecognitionException {
		TokenExpFunctionContext _localctx = new TokenExpFunctionContext(_ctx, getState());
		enterRule(_localctx, 10, RULE_tokenExpFunction);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(52);
			fuctionExp();
			setState(53);
			match(T__0);
			setState(54);
			match(ATTRIBUTE);
			setState(55);
			match(T__1);
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

	public static class FuctionExpContext extends ParserRuleContext {
		public TerminalNode TO_LOWER() { return getToken(NugetODataFilterParser.TO_LOWER, 0); }
		public FuctionExpContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_fuctionExp; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof NugetODataFilterListener ) ((NugetODataFilterListener)listener).enterFuctionExp(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof NugetODataFilterListener ) ((NugetODataFilterListener)listener).exitFuctionExp(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof NugetODataFilterVisitor ) return ((NugetODataFilterVisitor<? extends T>)visitor).visitFuctionExp(this);
			else return visitor.visitChildren(this);
		}
	}

	public final FuctionExpContext fuctionExp() throws RecognitionException {
		FuctionExpContext _localctx = new FuctionExpContext(_ctx, getState());
		enterRule(_localctx, 12, RULE_fuctionExp);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(57);
			match(TO_LOWER);
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

	public static class FilterOpContext extends ParserRuleContext {
		public TerminalNode EQ() { return getToken(NugetODataFilterParser.EQ, 0); }
		public TerminalNode GE() { return getToken(NugetODataFilterParser.GE, 0); }
		public FilterOpContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_filterOp; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof NugetODataFilterListener ) ((NugetODataFilterListener)listener).enterFilterOp(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof NugetODataFilterListener ) ((NugetODataFilterListener)listener).exitFilterOp(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof NugetODataFilterVisitor ) return ((NugetODataFilterVisitor<? extends T>)visitor).visitFilterOp(this);
			else return visitor.visitChildren(this);
		}
	}

	public final FilterOpContext filterOp() throws RecognitionException {
		FilterOpContext _localctx = new FilterOpContext(_ctx, getState());
		enterRule(_localctx, 14, RULE_filterOp);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(59);
			_la = _input.LA(1);
			if ( !(_la==EQ || _la==GE) ) {
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
		public TerminalNode AND() { return getToken(NugetODataFilterParser.AND, 0); }
		public TerminalNode OR() { return getToken(NugetODataFilterParser.OR, 0); }
		public LogicalOpContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_logicalOp; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof NugetODataFilterListener ) ((NugetODataFilterListener)listener).enterLogicalOp(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof NugetODataFilterListener ) ((NugetODataFilterListener)listener).exitLogicalOp(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof NugetODataFilterVisitor ) return ((NugetODataFilterVisitor<? extends T>)visitor).visitLogicalOp(this);
			else return visitor.visitChildren(this);
		}
	}

	public final LogicalOpContext logicalOp() throws RecognitionException {
		LogicalOpContext _localctx = new LogicalOpContext(_ctx, getState());
		enterRule(_localctx, 16, RULE_logicalOp);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(61);
			_la = _input.LA(1);
			if ( !(_la==AND || _la==OR) ) {
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
		case 1:
			return filterExp_sempred((FilterExpContext)_localctx, predIndex);
		}
		return true;
	}
	private boolean filterExp_sempred(FilterExpContext _localctx, int predIndex) {
		switch (predIndex) {
		case 0:
			return precpred(_ctx, 2);
		}
		return true;
	}

	public static final String _serializedATN =
		"\3\u608b\ua72a\u8133\ub9ed\u417c\u3be7\u7786\u5964\3\17B\4\2\t\2\4\3\t"+
		"\3\4\4\t\4\4\5\t\5\4\6\t\6\4\7\t\7\4\b\t\b\4\t\t\t\4\n\t\n\3\2\3\2\3\3"+
		"\3\3\3\3\3\3\3\3\3\3\5\3\35\n\3\3\3\3\3\3\3\3\3\7\3#\n\3\f\3\16\3&\13"+
		"\3\3\4\3\4\3\4\3\4\3\4\5\4-\n\4\3\5\3\5\3\5\3\5\3\6\3\6\5\6\65\n\6\3\7"+
		"\3\7\3\7\3\7\3\7\3\b\3\b\3\t\3\t\3\n\3\n\3\n\2\3\4\13\2\4\6\b\n\f\16\20"+
		"\22\2\4\3\2\t\n\3\2\13\f\2<\2\24\3\2\2\2\4\34\3\2\2\2\6,\3\2\2\2\b.\3"+
		"\2\2\2\n\64\3\2\2\2\f\66\3\2\2\2\16;\3\2\2\2\20=\3\2\2\2\22?\3\2\2\2\24"+
		"\25\5\4\3\2\25\3\3\2\2\2\26\27\b\3\1\2\27\30\7\3\2\2\30\31\5\4\3\2\31"+
		"\32\7\4\2\2\32\35\3\2\2\2\33\35\5\6\4\2\34\26\3\2\2\2\34\33\3\2\2\2\35"+
		"$\3\2\2\2\36\37\f\4\2\2\37 \5\22\n\2 !\5\4\3\5!#\3\2\2\2\"\36\3\2\2\2"+
		"#&\3\2\2\2$\"\3\2\2\2$%\3\2\2\2%\5\3\2\2\2&$\3\2\2\2\'(\5\n\6\2()\5\20"+
		"\t\2)*\5\b\5\2*-\3\2\2\2+-\7\7\2\2,\'\3\2\2\2,+\3\2\2\2-\7\3\2\2\2./\7"+
		"\5\2\2/\60\7\16\2\2\60\61\7\5\2\2\61\t\3\2\2\2\62\65\7\b\2\2\63\65\5\f"+
		"\7\2\64\62\3\2\2\2\64\63\3\2\2\2\65\13\3\2\2\2\66\67\5\16\b\2\678\7\3"+
		"\2\289\7\b\2\29:\7\4\2\2:\r\3\2\2\2;<\7\6\2\2<\17\3\2\2\2=>\t\2\2\2>\21"+
		"\3\2\2\2?@\t\3\2\2@\23\3\2\2\2\6\34$,\64";
	public static final ATN _ATN =
		new ATNDeserializer().deserialize(_serializedATN.toCharArray());
	static {
		_decisionToDFA = new DFA[_ATN.getNumberOfDecisions()];
		for (int i = 0; i < _ATN.getNumberOfDecisions(); i++) {
			_decisionToDFA[i] = new DFA(_ATN.getDecisionState(i), i);
		}
	}
}