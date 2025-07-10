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
import org.antlr.v4.runtime.atn.LexerATNSimulator;
import org.antlr.v4.runtime.atn.PredictionContextCache;
import org.antlr.v4.runtime.dfa.DFA;

@SuppressWarnings({"all", "warnings", "unchecked", "unused", "cast"})
public class AQLLexer extends Lexer {
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
	public static String[] channelNames = {
		"DEFAULT_TOKEN_CHANNEL", "HIDDEN"
	};

	public static String[] modeNames = {
		"DEFAULT_MODE"
	};

	public static final String[] ruleNames = {
		"LAYOUT_GITLFS", "LAYOUT_MAVEN", "LAYOUT_RAW", "LAYOUT_NPM", "LAYOUT_NUGET", 
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


	public AQLLexer(CharStream input) {
		super(input);
		_interp = new LexerATNSimulator(this,_ATN,_decisionToDFA,_sharedContextCache);
	}

	@Override
	public String getGrammarFileName() { return "AQL.g4"; }

	@Override
	public String[] getRuleNames() { return ruleNames; }

	@Override
	public String getSerializedATN() { return _serializedATN; }

	@Override
	public String[] getChannelNames() { return channelNames; }

	@Override
	public String[] getModeNames() { return modeNames; }

	@Override
	public ATN getATN() { return _ATN; }

	public static final String _serializedATN =
		"\3\u608b\ua72a\u8133\ub9ed\u417c\u3be7\u7786\u5964\2-\u0142\b\1\4\2\t"+
		"\2\4\3\t\3\4\4\t\4\4\5\t\5\4\6\t\6\4\7\t\7\4\b\t\b\4\t\t\t\4\n\t\n\4\13"+
		"\t\13\4\f\t\f\4\r\t\r\4\16\t\16\4\17\t\17\4\20\t\20\4\21\t\21\4\22\t\22"+
		"\4\23\t\23\4\24\t\24\4\25\t\25\4\26\t\26\4\27\t\27\4\30\t\30\4\31\t\31"+
		"\4\32\t\32\4\33\t\33\4\34\t\34\4\35\t\35\4\36\t\36\4\37\t\37\4 \t \4!"+
		"\t!\4\"\t\"\4#\t#\4$\t$\4%\t%\4&\t&\4\'\t\'\4(\t(\4)\t)\4*\t*\4+\t+\4"+
		",\t,\3\2\3\2\3\2\3\2\3\2\3\2\3\2\3\3\3\3\3\3\3\3\3\3\3\3\3\4\3\4\3\4\3"+
		"\4\3\5\3\5\3\5\3\5\3\6\3\6\3\6\3\6\3\6\3\6\3\7\3\7\3\7\3\7\3\7\3\b\3\b"+
		"\3\b\3\b\3\b\3\b\3\b\3\b\3\b\3\b\3\t\3\t\3\t\3\t\3\t\3\t\3\t\3\t\3\n\3"+
		"\n\3\n\3\n\3\n\3\n\3\n\3\n\3\n\3\n\3\n\3\13\3\13\3\13\3\13\3\13\3\13\3"+
		"\13\3\13\3\13\3\13\3\13\3\f\3\f\3\f\3\f\3\f\3\r\3\r\3\r\3\r\3\r\3\r\3"+
		"\r\3\r\3\r\3\r\3\16\3\16\3\16\3\16\3\16\3\16\3\17\3\17\3\17\3\17\3\17"+
		"\3\20\3\20\3\20\3\20\3\20\3\20\3\20\3\20\3\20\3\20\3\20\3\20\3\20\3\21"+
		"\3\21\3\21\3\22\3\22\3\22\3\22\3\22\3\23\3\23\3\23\3\23\3\23\3\23\3\23"+
		"\3\23\3\24\3\24\3\24\3\24\3\24\3\24\3\24\3\24\3\24\3\24\3\24\3\25\3\25"+
		"\3\25\3\25\3\25\3\25\3\25\3\26\3\26\3\26\3\26\3\26\3\26\3\26\3\26\3\27"+
		"\3\27\3\27\3\27\3\30\3\30\3\30\3\30\3\30\3\31\3\31\3\31\3\32\3\32\3\32"+
		"\3\32\3\33\3\33\3\33\3\33\3\34\3\34\3\34\3\34\3\34\3\35\3\35\3\35\3\35"+
		"\3\35\3\36\3\36\3\37\3\37\3\37\3 \3 \3 \3 \3!\3!\3\"\3\"\3\"\3#\3#\3#"+
		"\3$\3$\3%\3%\3&\3&\3\'\3\'\3(\3(\3)\6)\u012d\n)\r)\16)\u012e\3*\6*\u0132"+
		"\n*\r*\16*\u0133\3+\3+\7+\u0138\n+\f+\16+\u013b\13+\3+\3+\3,\3,\3,\3,"+
		"\2\2-\3\3\5\4\7\5\t\6\13\7\r\b\17\t\21\n\23\13\25\f\27\r\31\16\33\17\35"+
		"\20\37\21!\22#\23%\24\'\25)\26+\27-\30/\31\61\32\63\33\65\34\67\359\36"+
		";\37= ?!A\"C#E$G%I&K\'M(O)Q*S+U,W-\3\2\b\4\2##\u0080\u0080\3\2\62;\b\2"+
		",,/\60\62;C\\aac|\5\2$$))^^\7\2\f\f\17\17$$))^^\3\2\"\"\2\u0144\2\3\3"+
		"\2\2\2\2\5\3\2\2\2\2\7\3\2\2\2\2\t\3\2\2\2\2\13\3\2\2\2\2\r\3\2\2\2\2"+
		"\17\3\2\2\2\2\21\3\2\2\2\2\23\3\2\2\2\2\25\3\2\2\2\2\27\3\2\2\2\2\31\3"+
		"\2\2\2\2\33\3\2\2\2\2\35\3\2\2\2\2\37\3\2\2\2\2!\3\2\2\2\2#\3\2\2\2\2"+
		"%\3\2\2\2\2\'\3\2\2\2\2)\3\2\2\2\2+\3\2\2\2\2-\3\2\2\2\2/\3\2\2\2\2\61"+
		"\3\2\2\2\2\63\3\2\2\2\2\65\3\2\2\2\2\67\3\2\2\2\29\3\2\2\2\2;\3\2\2\2"+
		"\2=\3\2\2\2\2?\3\2\2\2\2A\3\2\2\2\2C\3\2\2\2\2E\3\2\2\2\2G\3\2\2\2\2I"+
		"\3\2\2\2\2K\3\2\2\2\2M\3\2\2\2\2O\3\2\2\2\2Q\3\2\2\2\2S\3\2\2\2\2U\3\2"+
		"\2\2\2W\3\2\2\2\3Y\3\2\2\2\5`\3\2\2\2\7f\3\2\2\2\tj\3\2\2\2\13n\3\2\2"+
		"\2\rt\3\2\2\2\17y\3\2\2\2\21\u0083\3\2\2\2\23\u008b\3\2\2\2\25\u0096\3"+
		"\2\2\2\27\u00a1\3\2\2\2\31\u00a6\3\2\2\2\33\u00b0\3\2\2\2\35\u00b6\3\2"+
		"\2\2\37\u00bb\3\2\2\2!\u00c8\3\2\2\2#\u00cb\3\2\2\2%\u00d0\3\2\2\2\'\u00d8"+
		"\3\2\2\2)\u00e3\3\2\2\2+\u00ea\3\2\2\2-\u00f2\3\2\2\2/\u00f6\3\2\2\2\61"+
		"\u00fb\3\2\2\2\63\u00fe\3\2\2\2\65\u0102\3\2\2\2\67\u0106\3\2\2\29\u010b"+
		"\3\2\2\2;\u0110\3\2\2\2=\u0112\3\2\2\2?\u0115\3\2\2\2A\u0119\3\2\2\2C"+
		"\u011b\3\2\2\2E\u011e\3\2\2\2G\u0121\3\2\2\2I\u0123\3\2\2\2K\u0125\3\2"+
		"\2\2M\u0127\3\2\2\2O\u0129\3\2\2\2Q\u012c\3\2\2\2S\u0131\3\2\2\2U\u0135"+
		"\3\2\2\2W\u013e\3\2\2\2YZ\7I\2\2Z[\7k\2\2[\\\7v\2\2\\]\7N\2\2]^\7h\2\2"+
		"^_\7u\2\2_\4\3\2\2\2`a\7o\2\2ab\7c\2\2bc\7x\2\2cd\7g\2\2de\7p\2\2e\6\3"+
		"\2\2\2fg\7T\2\2gh\7c\2\2hi\7y\2\2i\b\3\2\2\2jk\7p\2\2kl\7r\2\2lm\7o\2"+
		"\2m\n\3\2\2\2no\7p\2\2op\7w\2\2pq\7i\2\2qr\7g\2\2rs\7v\2\2s\f\3\2\2\2"+
		"tu\7r\2\2uv\7c\2\2vw\7v\2\2wx\7j\2\2x\16\3\2\2\2yz\7g\2\2z{\7z\2\2{|\7"+
		"v\2\2|}\7g\2\2}~\7p\2\2~\177\7u\2\2\177\u0080\7k\2\2\u0080\u0081\7q\2"+
		"\2\u0081\u0082\7p\2\2\u0082\20\3\2\2\2\u0083\u0084\7i\2\2\u0084\u0085"+
		"\7t\2\2\u0085\u0086\7q\2\2\u0086\u0087\7w\2\2\u0087\u0088\7r\2\2\u0088"+
		"\u0089\7K\2\2\u0089\u008a\7f\2\2\u008a\22\3\2\2\2\u008b\u008c\7e\2\2\u008c"+
		"\u008d\7n\2\2\u008d\u008e\7c\2\2\u008e\u008f\7u\2\2\u008f\u0090\7u\2\2"+
		"\u0090\u0091\7k\2\2\u0091\u0092\7h\2\2\u0092\u0093\7k\2\2\u0093\u0094"+
		"\7g\2\2\u0094\u0095\7t\2\2\u0095\24\3\2\2\2\u0096\u0097\7c\2\2\u0097\u0098"+
		"\7t\2\2\u0098\u0099\7v\2\2\u0099\u009a\7k\2\2\u009a\u009b\7h\2\2\u009b"+
		"\u009c\7c\2\2\u009c\u009d\7e\2\2\u009d\u009e\7v\2\2\u009e\u009f\7K\2\2"+
		"\u009f\u00a0\7f\2\2\u00a0\26\3\2\2\2\u00a1\u00a2\7r\2\2\u00a2\u00a3\7"+
		"c\2\2\u00a3\u00a4\7v\2\2\u00a4\u00a5\7j\2\2\u00a5\30\3\2\2\2\u00a6\u00a7"+
		"\7g\2\2\u00a7\u00a8\7z\2\2\u00a8\u00a9\7v\2\2\u00a9\u00aa\7g\2\2\u00aa"+
		"\u00ab\7p\2\2\u00ab\u00ac\7u\2\2\u00ac\u00ad\7k\2\2\u00ad\u00ae\7q\2\2"+
		"\u00ae\u00af\7p\2\2\u00af\32\3\2\2\2\u00b0\u00b1\7u\2\2\u00b1\u00b2\7"+
		"e\2\2\u00b2\u00b3\7q\2\2\u00b3\u00b4\7r\2\2\u00b4\u00b5\7g\2\2\u00b5\34"+
		"\3\2\2\2\u00b6\u00b7\7p\2\2\u00b7\u00b8\7c\2\2\u00b8\u00b9\7o\2\2\u00b9"+
		"\u00ba\7g\2\2\u00ba\36\3\2\2\2\u00bb\u00bc\7f\2\2\u00bc\u00bd\7k\2\2\u00bd"+
		"\u00be\7u\2\2\u00be\u00bf\7v\2\2\u00bf\u00c0\7t\2\2\u00c0\u00c1\7k\2\2"+
		"\u00c1\u00c2\7d\2\2\u00c2\u00c3\7w\2\2\u00c3\u00c4\7v\2\2\u00c4\u00c5"+
		"\7k\2\2\u00c5\u00c6\7q\2\2\u00c6\u00c7\7p\2\2\u00c7 \3\2\2\2\u00c8\u00c9"+
		"\7k\2\2\u00c9\u00ca\7f\2\2\u00ca\"\3\2\2\2\u00cb\u00cc\7v\2\2\u00cc\u00cd"+
		"\7{\2\2\u00cd\u00ce\7r\2\2\u00ce\u00cf\7g\2\2\u00cf$\3\2\2\2\u00d0\u00d1"+
		"\7u\2\2\u00d1\u00d2\7v\2\2\u00d2\u00d3\7q\2\2\u00d3\u00d4\7t\2\2\u00d4"+
		"\u00d5\7c\2\2\u00d5\u00d6\7i\2\2\u00d6\u00d7\7g\2\2\u00d7&\3\2\2\2\u00d8"+
		"\u00d9\7t\2\2\u00d9\u00da\7g\2\2\u00da\u00db\7r\2\2\u00db\u00dc\7q\2\2"+
		"\u00dc\u00dd\7u\2\2\u00dd\u00de\7k\2\2\u00de\u00df\7v\2\2\u00df\u00e0"+
		"\7q\2\2\u00e0\u00e1\7t\2\2\u00e1\u00e2\7{\2\2\u00e2(\3\2\2\2\u00e3\u00e4"+
		"\7n\2\2\u00e4\u00e5\7c\2\2\u00e5\u00e6\7{\2\2\u00e6\u00e7\7q\2\2\u00e7"+
		"\u00e8\7w\2\2\u00e8\u00e9\7v\2\2\u00e9*\3\2\2\2\u00ea\u00eb\7x\2\2\u00eb"+
		"\u00ec\7g\2\2\u00ec\u00ed\7t\2\2\u00ed\u00ee\7u\2\2\u00ee\u00ef\7k\2\2"+
		"\u00ef\u00f0\7q\2\2\u00f0\u00f1\7p\2\2\u00f1,\3\2\2\2\u00f2\u00f3\7v\2"+
		"\2\u00f3\u00f4\7c\2\2\u00f4\u00f5\7i\2\2\u00f5.\3\2\2\2\u00f6\u00f7\7"+
		"h\2\2\u00f7\u00f8\7t\2\2\u00f8\u00f9\7q\2\2\u00f9\u00fa\7o\2\2\u00fa\60"+
		"\3\2\2\2\u00fb\u00fc\7v\2\2\u00fc\u00fd\7q\2\2\u00fd\62\3\2\2\2\u00fe"+
		"\u00ff\7c\2\2\u00ff\u0100\7i\2\2\u0100\u0101\7g\2\2\u0101\64\3\2\2\2\u0102"+
		"\u0103\7c\2\2\u0103\u0104\7u\2\2\u0104\u0105\7e\2\2\u0105\66\3\2\2\2\u0106"+
		"\u0107\7f\2\2\u0107\u0108\7g\2\2\u0108\u0109\7u\2\2\u0109\u010a\7e\2\2"+
		"\u010a8\3\2\2\2\u010b\u010c\7u\2\2\u010c\u010d\7m\2\2\u010d\u010e\7k\2"+
		"\2\u010e\u010f\7r\2\2\u010f:\3\2\2\2\u0110\u0111\7(\2\2\u0111<\3\2\2\2"+
		"\u0112\u0113\7(\2\2\u0113\u0114\7(\2\2\u0114>\3\2\2\2\u0115\u0116\7c\2"+
		"\2\u0116\u0117\7p\2\2\u0117\u0118\7f\2\2\u0118@\3\2\2\2\u0119\u011a\7"+
		"~\2\2\u011aB\3\2\2\2\u011b\u011c\7~\2\2\u011c\u011d\7~\2\2\u011dD\3\2"+
		"\2\2\u011e\u011f\7q\2\2\u011f\u0120\7t\2\2\u0120F\3\2\2\2\u0121\u0122"+
		"\7-\2\2\u0122H\3\2\2\2\u0123\u0124\t\2\2\2\u0124J\3\2\2\2\u0125\u0126"+
		"\7<\2\2\u0126L\3\2\2\2\u0127\u0128\7*\2\2\u0128N\3\2\2\2\u0129\u012a\7"+
		"+\2\2\u012aP\3\2\2\2\u012b\u012d\t\3\2\2\u012c\u012b\3\2\2\2\u012d\u012e"+
		"\3\2\2\2\u012e\u012c\3\2\2\2\u012e\u012f\3\2\2\2\u012fR\3\2\2\2\u0130"+
		"\u0132\t\4\2\2\u0131\u0130\3\2\2\2\u0132\u0133\3\2\2\2\u0133\u0131\3\2"+
		"\2\2\u0133\u0134\3\2\2\2\u0134T\3\2\2\2\u0135\u0139\t\5\2\2\u0136\u0138"+
		"\n\6\2\2\u0137\u0136\3\2\2\2\u0138\u013b\3\2\2\2\u0139\u0137\3\2\2\2\u0139"+
		"\u013a\3\2\2\2\u013a\u013c\3\2\2\2\u013b\u0139\3\2\2\2\u013c\u013d\t\5"+
		"\2\2\u013dV\3\2\2\2\u013e\u013f\t\7\2\2\u013f\u0140\3\2\2\2\u0140\u0141"+
		"\b,\2\2\u0141X\3\2\2\2\6\2\u012e\u0133\u0139\3\b\2\2";
	public static final ATN _ATN =
		new ATNDeserializer().deserialize(_serializedATN.toCharArray());
	static {
		_decisionToDFA = new DFA[_ATN.getNumberOfDecisions()];
		for (int i = 0; i < _ATN.getNumberOfDecisions(); i++) {
			_decisionToDFA[i] = new DFA(_ATN.getDecisionState(i), i);
		}
	}
}